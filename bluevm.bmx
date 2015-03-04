
' Blue Moon
' Virtual Machine
' the VM allocates memory, manages bytecode and machine code, and interfaces between Max and Lua objects
' alongside the compiler, this more or less forms the toplevel accessible to the user

' due to recursive dependency problems, for the moment a lot of this is Include rather than Import based
' we'll fix that in post


SuperStrict

Import "bluecompiler.bmx"
Import "blueallocator.bmx"
Import "bluetable.bmx"

Rem
Import "blueasm.o"	'future project: assembly operators (this is a note, not an implementation)
Extern
Function ADD:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "ADD2"
Function EQ:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "EQ2"
'... you get the idea
End Extern
BlueJIT.InitOpTbl()
'BlueJIT.opTbl[BlueJIT.opc.ADD] = ADD
'BlueJIT.opTbl[BlueJIT.opc.EQ] = EQ
End Rem

Include "bluelibrary.bmx"
Include "bluejit.bmx"


' notes:
' - the structure of a bytecode object is:
'    [ machine code ptr ][ id-offset ][ #k ][ #param ][ #upvar ][ frame sz ][ #instrs ][ vm ][ instructions... ][ upvar table... ][ k table... ]
'   initial fields are all 32b; instructions are each 64b; upvar table is pairs of 32b values (level, pos); k table is NaN-box values

' - the structure of a closure object (actual Lua value representing a function) is:
'    [ bytecode ptr ][ upvar values... ]

Type BlueVM
	Field mem:BlueVMMemory, _ENV:BlueLuaVal
	Field idMod:Int, funIndex:Bytecode Ptr, _fiSz:Int
	Field strs:TMap, metaname:Byte Ptr[]
	
	Method New()
		mem = New BlueVMMemory ; strs = CreateMap()
		_ENV = NewTable()	'rebuild on each run?
		BlueBasicLibrary._Load(Self, _ENV)
	'	_ENV._vm = Null	'circular dependency (this does mean manipulating _ENV from outside could be problematic; we should probably have property accessors)
		
		Local opc:BlueOpcode = New BlueOpcode
		Local m:Byte Ptr[] = New Byte Ptr[opc.NUM_OPCODES]	'this table is used to ease metamethod access (centralize it)
		m[opc.GETTAB] = mem.AllocConstStr("__index") ; m[opc.GETTABSI] = m[opc.GETTAB] ; m[opc.GETTABI] = m[opc.GETTAB]
		m[opc.SETTAB] = mem.AllocConstStr("__newindex") ; m[opc.SETTABSI] = m[opc.SETTAB] ; m[opc.SETTABI] = m[opc.SETTAB]
		m[opc.ADD]  = mem.AllocConstStr("__add")
		m[opc.SUB]  = mem.AllocConstStr("__sub")
		m[opc.MUL]  = mem.AllocConstStr("__mul")
		m[opc.DIV]  = mem.AllocConstStr("__div")
		m[opc.NMOD] = mem.AllocConstStr("__mod")
		m[opc.POW]  = mem.AllocConstStr("__pow")
		m[opc.CAT]  = mem.AllocConstStr("__concat")
		m[opc.IDIV] = mem.AllocConstStr("__idiv")
		m[opc.BAND] = mem.AllocConstStr("__band")
		m[opc.BOR]  = mem.AllocConstStr("__bor")
		m[opc.BXOR] = mem.AllocConstStr("__bxor")
		m[opc.BSHL] = mem.AllocConstStr("__shl")
		m[opc.BSHR] = mem.AllocConstStr("__shr")
		m[opc.UNM]  = mem.AllocConstStr("__unm")
		m[opc.ALEN] = mem.AllocConstStr("__len")
		m[opc.BNOT] = mem.AllocConstStr("__bnot")
		m[opc.UNP]  = mem.AllocConstStr("__unp")	'damn right it's supported
		m[opc.EQ]   = mem.AllocConstStr("__eq")
		m[opc.LT]   = mem.AllocConstStr("__lt")
		m[opc.LEQ]  = mem.AllocConstStr("__le")
		m[opc.CALL] = mem.AllocConstStr("__call")
		metaname = m
	End Method
	
	' Load the procedures and constants of a compiled binary into the VM, returning the function representing the program toplevel
	Method LoadObjectCode:BlueLuaVal(code:BlueBinary)
		Local buf:Int[] = code.buf
		Local fcount:Int = buf[0], kcount:Int = buf[1], ktblpos:Int = buf[2], ktbl:Long[kcount]
		Local convert:Byte Ptr(o:Object) = Byte Ptr(BlueJIT.Identity)
		
		For Local k:Int = 0 Until kcount
			Local koff:Int = buf[ktblpos + k], sz:Int = buf[ktblpos + k + 1] - koff
			If sz > 2	'strings have size > 2
				Local length:Double, lp:Int Ptr = Int Ptr(Varptr(length))
				lp[0] = buf[koff] ; lp[1] = buf[koff + 1]
				Local s:Byte Ptr = mem.AllocConstant(length, Short Ptr(Varptr(buf[koff + 2])))
				ktbl[k] = mem.PtrToVal(s, BlueTypeTag.STR)
			Else
				Local d:Double, dp:Int Ptr = Int Ptr(Varptr(d))
				dp[0] = buf[koff] ; dp[1] = buf[koff + 1]
				Double Ptr(Varptr(ktbl[k]))[0] = d
			EndIf
		Next
		
		ExtendFunIndex(idMod + fcount)
		For Local f:Int = 0 Until fcount
			Local foff:Int = buf[3 + f]
			Local ic:Int = buf[foff + 1], kc:Int = buf[foff + 2], pc:Int = buf[foff + 3], uc:Int = buf[foff + 5], fsz:Int = buf[foff + 6]
			
			Local b:Bytecode
			Repeat
				Local bp:Byte Ptr = mem.AllocBytecode(uc + uc Mod 2, kc, ic) ; b = BPtoBC(bp)
			Until True
			b.idMod = idMod ; b.kcount = kc ; b.pcount = pc ; b.upvars = uc + uc Mod 2	'round up for alignment
			b.frameSz = mem.STACKFRAMESZ + b.upvars * 8 + fsz * 8 ; b.icount = ic ; b.vm = convert(Self)
			
			Local ib:Int Ptr = Int Ptr(Byte Ptr(b) + 4) + 8 + 2 * ic
			For Local u:Int = 0 Until uc
				Local uoff:Int = 7 + u * 2
				ib[u * 2] = buf[foff + uoff] ; ib[u * 2 + 1] = buf[foff + uoff + 1]
			Next
			If uc <> b.upvars	'ensure padding is safe by copying the first one
				ib[uc * 2] = buf[foff + 7] ; ib[uc * 2 + 1] = buf[foff + 7 + 1]
			EndIf
			
			Local db:Long Ptr = Long Ptr(ib + 2 * b.upvars)
			For Local k:Int = 0 Until kc
				db[k] = ktbl[buf[foff + 7 + uc * 2 + k]]
			Next
			
			Local ioff:Int = 7 + 2 * uc + kc ; ib = Int Ptr(Byte Ptr(b) + BlueJIT.BYTECODE_INC)
			
			For Local i:Int = 0 Until ic * 2
				ib[i] = buf[foff + ioff + i]
			Next
			
			b.mcode = BlueJIT.Compile(Self, ib, ic, b) + BlueJIT.PROLOGUESZ
			funIndex[idMod + f] = b
		Next
		
		idMod :+ fcount
		Return New BlueLuaVal
	End Method
	
	Method ExtendFunIndex(sz:Int)	'hack: BlitzMax has a bug with arrays of extern types! so we need to use C-style arrays for now
		Local newFI:Bytecode Ptr = Bytecode Ptr(MemAlloc(sz * 4))
		If _fiSz
			For Local b:Int = 0 Until _fiSz
				newFI[b] = funIndex[b]
			Next
			MemFree(funIndex)
			For Local b:Int = _fiSz Until sz
				newFI[b] = Null
			Next
		EndIf
		funIndex = newFI ; _fiSz = sz
	End Method
	
	Method CallToLua()
	End Method
	
	Method Error(msg:String)
		Throw "[temporary error framework]: " + msg
	End Method
	
	Method NewTable:BlueLuaVal()
		Local t:Byte Ptr = mem.AllocTable(Null), ret:BlueLuaVal = BlueLuaVal.Make(Self)
		Byte Ptr Ptr(Varptr(ret.val))[0] = t ; Int Ptr(Varptr(ret.val))[1] = BlueTypeTag.NANBOX | BlueTypeTag.TBL
		ret._obj = mem.RootObj(t)
		Return ret
	End Method
	
	Method ValueFromObject:BlueLuaVal(o:Object)
	End Method
	Method ValueFromNumber:BlueLuaVal(n:Double)
		Local v:BlueLuaVal = New BlueLuaVal
		Double Ptr(Varptr(v.val))[0] = n
		Return v
	End Method
	Method ValueFromString:BlueLuaVal(s:String)
	End Method
	Method ValueFromBinary:BlueLuaVal(b:Byte Ptr)
	End Method
	Method ValueFromFunction:BlueLuaVal(f:Byte Ptr)
		Local v:BlueLuaVal = New BlueLuaVal
		Byte Ptr Ptr(Varptr(v.val))[0] = f
		Int Ptr(Varptr(v.val))[1] = BlueTypeTag.NANBOX | BlueTypeTag.NATFUN
		Return v
	End Method
	
	Method ValueFromLua:BlueLuaVal(val:Byte Ptr, tag:Int)
		Local ret:BlueLuaVal = BlueLuaVal.Make(Self)
		Byte Ptr Ptr(Varptr(ret.val))[0] = val ; Int Ptr(Varptr(ret.val))[1] = tag
		ret._obj = mem.RootObj(val)
		Return ret
	End Method
	
	Global BPtoBC:Bytecode(p:Byte Ptr) = Byte Ptr(BlueJIT.PointerToExtType)
End Type

Type BlueLuaVal
	Field _obj:BlueGCNode, val:Long, _vm:BlueVM
	
	Method Call:BlueLuaVal(arg:BlueLuaVal[])
	End Method
	
	Method Set(f:String, val:BlueLuaVal)
		'need to add type checks to these operators
		Local k:BlueGCNode = BlueGCNode(_vm.strs.ValueForKey(f)), ks:Long
		If k
			Byte Ptr Ptr(Varptr(ks))[0] = k.val ; Int Ptr(Varptr(ks))[1] = BlueTypeTag.NANBOX | BlueTypeTag.STR
		Else
			ks = _vm.mem.MaxStringToVal(f)
			k = _vm.mem.RootObj(Byte Ptr(Int(ks)))
			_vm.strs.Insert(f, k)
		EndIf
		BlueTable.RawSet(_vm.mem, _obj.val, ks, val.val)
	End Method
	Method Get:BlueLuaVal(f:String)
		Local k:BlueGCNode = BlueGCNode(_vm.strs.ValueForKey(f)), ks:Long
		If k
			Byte Ptr Ptr(Varptr(ks))[0] = k.val ; Int Ptr(Varptr(ks))[1] = BlueTypeTag.NANBOX | BlueTypeTag.STR
			Local ret:Long = BlueTable.RawGet(_obj.val, ks), tag:Int = Int Ptr(Varptr(ret))[1]
			If tag = (BlueTypeTag.NANBOX | BlueTypeTag.NIL)
				Return Null
			ElseIf tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX
				Return _vm.ValueFromNumber(Double Ptr(Varptr(ret))[0])
			Else
				Local v:BlueLuaVal = New Self
				v.val = ret ; v._vm = _vm ; v._obj = _vm.mem.RootObj(Byte Ptr Ptr(Varptr(ret))[0])
				Return v
			EndIf
		Else
			Return Null
		EndIf
	End Method
	
	Method Unpin()
		If _obj Then _obj = Null ; val = 0
	End Method
	Function Make:BlueLuaVal(vm:BlueVM)
		Local v:BlueLuaVal = New Self
		v._vm = vm
		Return v
	End Function
	Method Delete()
		If _obj Then _obj.Remove()
	End Method
End Type

