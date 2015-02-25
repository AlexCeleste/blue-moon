
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

' - bigSpc is a simple list of conventionally-allocated objects that don't fit on a 1M page

' - objects use this layout:
'    [ 64b header: [ 32b size ][ 16b type tag ][ 8b colour ][ 1b hasFinalizer ][ 2b weak keys, values ] ][ N * 64b data area... ]
'   size includes header. for moved objects the second 32b are replaced by a forwarding pointer
'   pointers in general are to the data area, skipping header

' - data types are:  nil, boolean, number, string, closure, natfun, userdata, thread, table ; upvar, tablearray, tablehash, bytecode
'   nil/boolean/number/natfun are value types and need no allocation
'   string uses this structure:
'    [H][ 32b length ][ 32b hash ][ ceil(length/4) * 64b chars... ]
'   closure uses this structure:
'    [H][ 32b bytecode ptr ][32b][ N * 32b upvar ptrs... ] (N rounded up to 2 for alignment)
'   userdata uses this structure:
'    [H][ 32b metatable ][ 32b value ]
'   thread uses this structure:
'    [H][
'   table uses this structure:
'    [H][ 32b metatable ][32b][ 32b hashpart ptr + 8 ][ 32b arraypart ptr + 8 ]
'   tablearray uses this structure:
'    [H][ 32b len ][ 32b maxlen ][ N * 64b values... ]
'   tablehash uses this structure:
'    [H][ 32b count ][ 32b log2(maxlen) ][ N * [ 64b key ][ 64b value ]... ]
'   upvar uses this structure:
'    [H][ 64b value ]
'   bytecode uses this structure:
'    [H][


SuperStrict

Import "blueerror.bmx"
Import "bluememory.c"

Private
Extern
	Function PageAlloc:Byte Ptr(size:Int) = "bluemoon_mmap"
	Function PageSetRW:Int(p:Byte Ptr, sz:Int) = "bluemoon_mprotect_rw"
	Function PageSetRWX:Int(p:Byte Ptr, sz:Int) = "bluemoon_mprotect_rwx"
	Function PageSetProtected:Int(p:Byte Ptr, sz:Int) = "bluemoon_mprotect_none"
	Function PageFree:Int(p:Byte Ptr, sz:Int) = "bluemoon_munmap"
End Extern
Public


Type BlueTypeTag Final
	Const NUM:Int = 0, NIL:Int = 1, BOOL:Int = 1 Shl 1, STR:Int = 1 Shl 2, FUN:Int = 1 Shl 3
	Const NATFUN:Int = 1 Shl 4, USR:Int = 1 Shl 5, THR:Int = 1 Shl 6, TBL:Int = 1 Shl 7
	Const UPV:Int = 1 Shl 8, ARR:Int = 1 Shl 9, HASH:Int = 1 Shl 10, BCODE:Int = 1 Shl 11
	
	Const NANBOX:Int = Int(2^11 - 1) Shl 20, NANBOX_CHK:Int = NANBOX | (1 Shl 19)	'test against upper word (and that it's not a genuine NaN)
End Type

Type BlueVMMemory Final
	Const PAGESZ:Int = 1048576, PAGEBITMAPSZ:Int = 16384, PAGEMETASZ:Int = 256
	Const EDENSIZE:Int = 8 * PAGESZ, STACKSZ:Int = 8 * PAGESZ, FUNCSIZE:Int = 120, BIGOBJECTSZ:Int = 500000
		
	Field gcroots:BlueGCNode, stack:Byte Ptr
	Field newSpace:Byte Ptr, cpySpace:Byte Ptr, oldPtrSpace:Byte Ptr[], oldStrSpace:Byte Ptr[], bigSpace:Byte Ptr[]
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
		newSpace = AlignedAlloc(EDENSIZE, PAGESZ) ; cpySpace = AlignedAlloc(EDENSIZE, PAGESZ)
		newPtr = 0
		
		AddCodePage()
		AddPage(oldStrSpace, PAGEMETASZ)
		AddPage(oldPtrSpace, PAGEBITMAPSZ)
	End Method
	Method Delete()
		' unmap a bunch of stuff
	End Method
	
	Method Write(slot:Long Ptr, val:Long)	'write barrier around old/big-space
		'implement it here
		slot[0] = val
	End Method
	
	Method Collect()
	End Method
	
	Method AllocTable:Byte Ptr(meta:Byte Ptr)
		Local ret:Byte Ptr = AllocObject(16, BlueTypeTag.TBL)
		Byte Ptr Ptr(ret)[0] = meta
		Int Ptr(ret)[2] = 0 ; Int Ptr(ret)[3] = 0
		Return ret
	End Method
	Method AllocClosure:Byte Ptr(bc:Byte Ptr, upvars:Int)	'already aligned
		Local ret:Byte Ptr = AllocObject(8 + upvars * 4, BlueTypeTag.FUN)
		Byte Ptr Ptr(ret)[0] = bc
		Return ret
	End Method
	Method AllocString:Byte Ptr(size:Int, charp:Short Ptr)
		Local sz:Int = size
		If sz Mod 4 Then sz :+ 4 - sz Mod 4	'align properly
		Local ret:Byte Ptr = AllocObject(8 + sz * 2, BlueTypeTag.STR), destp:Short Ptr = Short Ptr(ret + 8), hash:Int = 5381
		For Local c:Int = 0 Until size	'fill the string and compute hash in one go
			destp[c] = charp[c]
			hash = (hash * 33) ~ charp[c]	'djb2
		Next
		Int Ptr(ret)[0] = size ; Int Ptr(ret)[1] = hash
		Return ret
	End Method
	Method AllocUserdata:Byte Ptr(meta:Byte Ptr, val:Byte Ptr)
		Local ret:Byte Ptr = AllocObject(8, BlueTypeTag.USR)
		Byte Ptr Ptr(ret)[0] = meta ; Byte Ptr Ptr(ret)[1] = val
		Return ret
	End Method
	Method AllocThread()
	End Method
	
	Method AddCodePage()
		codeSpace = [AlignedAlloc(2 * PAGESZ, PAGESZ)] + codeSpace
		PageSetRWX(codeSpace[0], PAGESZ)
		Int Ptr(codeSpace[0])[0] = PAGEMETASZ
	End Method
	Method AddPage:Byte Ptr(space:Byte Ptr[] Var, init:Int, exec:Int = False)
		Local p:Byte Ptr = AlignedAlloc(PAGESZ, PAGESZ)	'x86: assume this returns zero-initialized result already
		space = [p] + space ; Int Ptr(p)[0] = init
		If exec Then PageSetRWX(space[0], PAGESZ)
		Return p
	End Method
	Method HeaderSize:Int(space:Byte Ptr[])
		Select space
			Case oldPtrSpace ; Return PAGEBITMAPSZ
			Default          ; Return PAGEMETASZ
		End Select
	End Method
	
	Method AllocCodeBlock:Int[](sz:Int)	'this can return less than the requested size; up to the JIT to request more
		Local page:Byte Ptr = codeSpace[0], pNewPtr:Int = Int Ptr(page)[0]
		If pNewPtr + sz > PAGESZ
			AddCodePage() ; Int Ptr(page)[0] = PAGESZ
			Return [Int(page) + pNewPtr, PAGESZ - pNewPtr]
		Else
			Int Ptr(page)[0] :+ sz
			Return [Int(page) + pNewPtr, sz]
		EndIf
	End Method
	
	Method AllocObject:Byte Ptr(sz:Int, tag:Int)
		sz :+ 8 ; Local ret:Byte Ptr
		If sz < BIGOBJECTSZ
			If newPtr + sz > EDENSIZE Then Collect()
			ret = newSpace + newPtr; newPtr :+ sz
		Else
			ret = MemAlloc(sz)
			If Not ret Then Throw BlueInterpretError.Make("unable to allocate memory for object")
			bigSpace :+ [ret]
			' track as part of allocated memory
		EndIf
		Int Ptr(ret)[0] = sz
		Return ret + 8
	End Method
	Method AllocObjectOldSpace:Byte Ptr(space:Byte Ptr[] Var, sz:Int, tag:Int)
		sz :+ 8 ; If sz Mod 8 Then sz :+ (8 - sz Mod 8)
		Local ret:Byte Ptr, page:Byte Ptr = space[0]
		Local pNewPtr:Int = Int Ptr(page)[0]
		If pNewPtr + sz > PAGESZ
			page = AddPage(space, HeaderSize(space)) ; pNewPtr = Int Ptr(page)[0]
		EndIf
		ret = page + pNewPtr ; Int Ptr(page)[0] = pNewPtr + sz
		Int Ptr(ret)[0] = sz
		Return ret + 8
	End Method
	
	Function PtrToVal:Long(p:Byte Ptr, tag:Int)
		Local ret:Long, rp:Int Ptr = Int Ptr(Varptr(ret))
		rp[0] = Int(p) ; rp[1] = BlueTypeTag.NANBOX | tag
		Return ret
	End Function
	Function ValToPtr:Byte Ptr(v:Long)
		Local vp:Byte Ptr Ptr = Byte Ptr Ptr(Varptr(v))
		Return vp[0]
	End Function
End Type

Type BlueGCNode
	Field pv:BlueGCNode, nx:BlueGCNode, val:Byte Ptr
	Method Remove()
		If pv Then pv.nx = nx ; pv = Null
		If nx Then nx.pv = pv ; nx = Null
	End Method
End Type

Private

Public

