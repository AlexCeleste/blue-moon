
' Blue Moon
' Memory allocator/garbage collector
' this is a generational three-colour mark/compact collector

' notes:

' - a basic Lua value is represented as a NaN-box (i.e. a double, unless it's a NaN in which case there's a pointer in the significand)

' - objects are allocated in newSpc, Cheney'd over to cpySpc when it runs out; the Eden spaces are 8MB each
' - when less than .edenThreshold remains in cpySpc after a scavenge, objects held from oldSpc are promoted
' - when more than .sizeThreshold has been promoted or less than .edenThreshold still remains, a full GC runs
'   this scan updates usage statistics for each page
'   the "worst" N pages are then compacted; if total usage is still poor, mark for another compact after next scavenge
'   the rest of Eden is then promoted
' - fallback to compacting in one go if usage is catastrophic/increments aren't keeping up
' - roots are obtained from the gcroots list, and the stack

' - before compacting/discarding, the space is scanned for objects with finalizers which are resurrected into newSpc and put on toFinalize
' - during a FULL collection, weak tables are listed in .weakTables rather than colouring elems, then weakscanned before compaction

' - the stack is an 8M block with the last page protected
' - stack frames use this format:
'    [ ret ip ][ prev base ][[ 16b frame sz ][ 16b var ofs ]][ func obj ][ ip ][ argv ][ retv ][[ 16b argc ][ 16b retc ]][ upv0 | upv1 | ... ][ v0 | v1 | ... ][ tmp... ]
'   the upvar slots, var slots, and temp slots beyond are all 64-bit NaN-boxes
'   the preceding slots are 32-bit words except otherwise shown

' - pages in oldPtrSpc use this format:
'    [[[  0..256 metadata  ]  ..16K dirty bitmap  ]  ..1M allocated objects  ]
'   each 64-byte potential NaN-box in the object-space has a corresponding dirty bit that is set if it points to newSpc
'   a metamap in the metadata in turn marks dirty sectors of the bitmap, for reduced scanning
'   metadata also contains allocation pointer and used space total

' - codeSpace stores executable instruction buffers for Lua functions (bytecode is allocated directly into oldStrSpc)
' - all instruction buffers are the same size; either the function is continued after a JMP, or the tail is unused
'   this means it can form a freelist without needing compaction (or be compacted easily b/c single-ownership)

' - bigSpc is a simple list of conventionally-allocated objects that don't fit on a 1M page
' - tables too big to fit on one page are split rather than allocated in bigSpc (~160G maximum)

' - objects use this layout:
'    [ 64b header: [ 32b size ][ 2b colour ][ 1b hasFinalizer ][ 2b weak keys, values ][ 11b type tag ] ][ N * 64b data area... ]
'   size includes header. for moved objects the second 32b are replaced by a forwarding pointer

' - data types are:  nil, boolean, number, string, function, userdata, thread, table ; upvar, tablearray, bigtable, bytecode
'   userdata is divided into heavy and light subtypes
'   function is divided into native and Lua subtypes
' - subtypes need to respond to both `tag & TYPE` and `tag & SUBTYPE`, so they need a two-bit pattern

SuperStrict

Import "blueerror.bmx"
Import "bluememory.c"

Private
Extern
	Function PageAlloc:Byte Ptr(size:Int) = "bluemoon_mmap"
	Function PageSetRW:Int(p:Byte Ptr, sz:Int) = "bluemoon_mprotect_rw"
	Function PageSetRWX:Int(p:Byte Ptr, sz:Int) = "bluemoon_mprotect_rwx"
	Function PageFree:Int(p:Byte Ptr, sz:Int) = "bluemoon_munmap"
End Extern
Public


Type BlueTypeTag Final
	Const OFS:Int = 5
	Const NUM:Int = 0, NIL:Int = 1 Shl OFS, BOOL:Int = 1 Shl (OFS + 1), STR:Int = 1 Shl (OFS + 2), FUN:Int = 1 Shl (OFS + 3)
	Const USR:Int = 1 Shl (OFS + 4), THR:Int = 1 Shl (OFS + 5), TBL:Int = 1 Shl (OFS + 6), UPV:Int = 1 Shl (OFS + 7)
	Const ARR:Int = 1 Shl (OFS + 8), BIG:Int = 1 Shl (OFS + 9), BCODE:Int = 1 Shl (OFS + 10), SUBTYPE:Int = 1 Shl (OFS + 11)
	Const HEAVY:Int = USR | SUBTYPE, NATFUN:Int = FUN | SUBTYPE
	
	Const NANBOX:Int = Int(2^11 - 1) Shl 20	'test against upper word
End Type

Type BlueVMMemory Final
	Const PAGESZ:Int = 1048576, PAGEBITMAPSZ:Int = 16384, PAGEMETASZ:Int = 256
	Const EDENSIZE:Int = 8 * PAGESZ, STACKSZ:Int = 8 * PAGESZ, FUNCSIZE:Int = 120, BIGOBJECTSZ:Int = 500000
		
	Field gcroots:BlueGCNode, stack:Byte Ptr
	Field newSpc:Byte Ptr, cpySpc:Byte Ptr, oldPtrSpc:Byte Ptr[], oldStrSpc:Byte Ptr[], bigSpc:Byte Ptr[]
	Field toFinalize:BlueGCNode, weakTables:BlueGCNode
	Field codeSpace:Byte Ptr[], codeFreeList:Byte Ptr
	
	Field memAlloced:Int
	Field edenThreshold:Int, sizeThreshold:Int
	Field newPtr:Int
	
	Function AlignedAlloc:Byte Ptr(size:Int, align:Int)	'abuse mmap/munmap to get greater-than-4K alignments
		?Not x86
		RuntimeError "The Blue Moon memory system does not support your platform (x86-32 only at this time)"
		?
		Local mem:Byte Ptr = PageAlloc(size + align)
		If Not mem Then Throw BlueInterpretError.Make("unable to allocate enough memory for system")
		
		' this is where it starts to get unpleasantly x86-32 specific
		Local iMem:Int = Int(mem), offset:Int = iMem Mod align
		If offset
			Local preceding:Int = align - offset
			PageFree(mem, preceding) ; mem :+ preceding
			PageFree(mem + size, offset)	'so we free the surrounding chunks of mem. that aren't aligned, and keep the middle
		Else
			PageFree(mem + size, align)
		EndIf
		
		Return mem
	End Function
	
	Method New()
		stack = AlignedAlloc(STACKSZ, PAGESZ)
		' add protection to end of stack
		newSpc = AlignedAlloc(EDENSIZE, PAGESZ) ; cpySpc = AlignedAlloc(EDENSIZE, PAGESZ)
		newPtr = 0
		
		codeFreeList = Byte Ptr(0)
		
		AddOldStrPage()	'we'll allocate bytecode straight into this, so it's needed
		AddOldPtrPage()	'may as well never check for null
	End Method
	
	Method AddCodePage()
		codeSpace = [AlignedAlloc(PAGESZ, PAGESZ)] + codeSpace
		PageSetRWX(codeSpace[0], PAGESZ)
		Local last:Int = PAGESZ - (FUNCSIZE + (PAGESZ Mod FUNCSIZE))
		For Local blk:Int = 0 Until last Step FUNCSIZE
			Byte Ptr Ptr(codeSpace[0] + blk)[0] = (codeSpace[0] + blk + FUNCSIZE)
		Next
		Byte Ptr Ptr(codeSpace[0] + last)[0] = codeFreeList
		codeFreeList = codeSpace[0]
	End Method
	Method AddOldPtrPage:Byte Ptr()
		Local l:Int = oldPtrSpc.Length, p:Byte Ptr = AlignedAlloc(PAGESZ, PAGESZ)
		oldPtrSpc = [p] + oldPtrSpc
		oldPtrSpc[l] = p
		Int Ptr(p)[0] = PAGEBITMAPSZ
		Return p
	End Method
	Method AddOldStrPage:Byte Ptr()
		Local l:Int = oldStrSpc.Length, p:Byte Ptr = AlignedAlloc(PAGESZ, PAGESZ)
		oldStrSpc = [p] + oldStrSpc
		oldStrSpc[l] = p
		Int Ptr(p)[0] = PAGEMETASZ
		Return p
	End Method
	
	Method AllocCodeBlock:Byte Ptr()
		If Not codeFreeList Then AddCodePage()
		Local b:Byte Ptr = codeFreeList
		codeFreeList = Byte Ptr Ptr(codeFreeList)[0]
		Return b
	End Method
	Method FreeCodeBlock(b:Byte Ptr)
		Byte Ptr Ptr(b)[0] = codeFreeList
		codeFreeList = b
	End Method
	
	Method AllocObject:Byte Ptr(sz:Int, tag:Int)
		sz :+ 8 ; Local ret:Byte Ptr
		If sz < BIGOBJECTSZ
		'	If newPtr + sz > EDENSIZE Then Collect()
			ret = newSpc + newPtr; newPtr :+ sz
		Else
			ret = MemAlloc(sz)
			If Not ret Then Throw BlueInterpretError.Make("unable to allocate memory for object")
			bigSpc :+ [ret]
			' track as part of allocated memory?
		EndIf
		Int Ptr(ret)[0] = sz
		Return ret + 8
	End Method
	Method AllocObjectOldPtr:Byte Ptr(sz:Int, tag:Int)
		sz :+ 8 ; If sz Mod 8 Then sz :+ (8 - sz Mod 8)
		Local ret:Byte Ptr, page:Byte Ptr = oldPtrSpc[0]
		Local pNewPtr:Int = Int Ptr(page)[0]
		If pNewPtr + sz > PAGESZ
			page = AddOldPtrPage() ; pNewPtr = Int Ptr(page)[0]
		EndIf
		ret = page + pNewPtr ; Int Ptr(page)[0] = pNewPtr + sz
		Int Ptr(ret)[0] = sz
		Return ret + 8
	End Method
	Method AllocObjectOldStr:Byte Ptr(sz:Int, tag:Int)
		sz :+ 8 ; If sz Mod 8 Then sz :+ (8 - sz Mod 8)
		Local ret:Byte Ptr, page:Byte Ptr = oldStrSpc[0]
		Local pNewPtr:Int = Int Ptr(page)[0]
		If pNewPtr + sz > PAGESZ
			page = AddOldStrPage() ; pNewPtr = Int Ptr(page)[0]
		EndIf
		ret = page + pNewPtr ; Int Ptr(page)[0] = pNewPtr + sz
		Int Ptr(ret)[0] = sz
		Return ret + 8
	End Method
	
	Function PtrToVal:Double(p:Byte Ptr, tag:Int)
		Local ret:Double, rp:Int Ptr = Int Ptr(Varptr(ret))
		rp[0] = Int(p) ; rp[1] = BlueTypeTag.NANBOX | tag
		Return ret
	End Function
	Function ValToPtr:Byte Ptr(v:Double)
		Local vp:Byte Ptr Ptr = Byte Ptr Ptr(Varptr(v))
		Return vp[0]
	End Function
End Type

Type BlueGCNode
	Field pv:BlueGCNode, nx:BlueGCNode, val:Byte Ptr
	Method Remove()
		If pv Then pv.nx = nx
		If nx Then nx.pv = pv
	End Method
End Type

Private

Public

