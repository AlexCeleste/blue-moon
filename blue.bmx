
' Blue Moon
' an implementation of Lua in pure BlitzMax

SuperStrict

'Framework Brl.Blitz
'Import Brl.LinkedList
'Import Brl.Map
'Import Brl.Reflection

Import "bluecompiler.bmx"
Import "blueallocator.bmx"
Import "bluetable.bmx"
Rem
Import "blueasm.o"
Extern
Function ADD:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "ADD2"
Function EQ:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "EQ2"
Function GETUPV:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "GETUPV2"
Function JIF:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "JIF2"
Function LOADBOOL:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "LOADBOOL2"
Function LOADK:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "LOADK2"
Function LOADNIL:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "LOADNIL2"
Function LOADSI:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "LOADSI2"
Function MOV:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "MOV2"
Function NEWUPV:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "NEWUPV2"
Function SETLC:Int(s:Stack, b:Bytecode, r:Byte Ptr) = "SETLC2"
End Extern
End Rem
'Const file:String = "tests/checktable.lua"
'Const file:String = "tests/mandelbrot.lua"
'Const file:String = "tests/sieve.lua"
'Const file:String = "tests/attrib.lua"
Const file:String = "test1.lua"

Const outFile:String = "out.lua.so"


Print BlueCompiler.ShowBytecode(file)

Rem
BlueJIT.InitOpTbl()
'BlueJIT.opTbl[BlueJIT.opc.ADD] = ADD
'BlueJIT.opTbl[BlueJIT.opc.EQ] = EQ
'BlueJIT.opTbl[BlueJIT.opc.GETUPV] = GETUPV
'BlueJIT.opTbl[BlueJIT.opc.JIF] = JIF
'BlueJIT.opTbl[BlueJIT.opc.LOADBOOL] = LOADBOOL
'BlueJIT.opTbl[BlueJIT.opc.LOADK] = LOADK
'BlueJIT.opTbl[BlueJIT.opc.LOADNIL] = LOADNIL
'BlueJIT.opTbl[BlueJIT.opc.LOADSI] = LOADSI
BlueJIT.opTbl[BlueJIT.opc.MOV] = MOV
'BlueJIT.opTbl[BlueJIT.opc.NEWUPV] = NEWUPV
'BlueJIT.opTbl[BlueJIT.opc.SETLC] = SETLC
End Rem

Local code:BlueBinary = BlueCompiler.CompileFileForLoad(file)
Local vm:BlueVM = New BlueVM
Local tl:BlueLuaVal = vm.LoadObjectCode(code)

Local stk:Stack = BlueJIT.BPtoS(vm.mem.stack)
stk.retIP = Null ; stk.prevBase = Null
Local vc:Int = 10, upvars:Int = 1
stk.varp = Long Ptr(Byte Ptr(stk) + BlueJIT.STACKFRAME_INC) + upvars
stk.func = vm.funIndex[0]
stk.argv = Null	'may want to add space?
stk.retv = Null
stk.argc = 0
stk.retc = 0

Function lpr(vm:BlueVM, argc:Int, argv:Long Ptr, retv:Long Ptr)
	Print vm.mem.ValToMaxString(argv[0])
End Function
vm._ENV.Set("pr", vm.ValueFromFunction(lpr))
vm._ENV.Set("quux", vm.ValueFromNumber(7.5))

Print "running..."
Local t:Int = MilliSecs()
Local test:Int(_:Byte Ptr) = stk.func.mcode - BlueJIT.PROLOGUESZ ; test(stk)
t = MilliSecs() - t
Print t
Print "run complete"

Rem
Local tbl:BlueLuaVal = vm.NewTable(), n:BlueLuaVal = vm.ValueFromNumber(6.5)
tbl.Set("foo", n) ; tbl.Set("bar", vm.ValueFromNumber(7.5)) ; tbl.Set("baz", vm.ValueFromNumber(9.7:Double))
Local res:BlueLuaVal = tbl.Get("foo")
Print Double Ptr(Varptr(res.val))[0]
res = tbl.Get("bar") ; Print Double Ptr(Varptr(res.val))[0]
res = tbl.Get("baz") ; Print Double Ptr(Varptr(res.val))[0]
End Rem

Print "done."
End

' notes:
' - string constants and bytecode are loaded directly into oldSpc, since they'll be needed often
' - the structure of a bytecode object is:
'    [ machine code ptr ][ id-offset ][ #k ][ #param ][ #upvar ][ frame sz ][ #instrs ][ vm ][ instructions... ][ upvar table... ][ k table... ]
'   initial fields are all 32b; instructions are each 64b; upvar table is pairs of 32b values (level, pos); k table is NaN-box values

' - the structure of a closure object (actual Lua value representing a function) is:
'    [ bytecode ptr ][ upvar values... ]

Type BlueVM
	Field mem:BlueVMMemory, _ENV:BlueLuaVal
	Field idMod:Int, funIndex:Bytecode Ptr, _fiSz:Int
	Field strs:TMap
	
	Method New()
		mem = New BlueVMMemory ; strs = CreateMap()
		_ENV = NewTable()	'rebuild on each run?
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
	
	Global BPtoBC:Bytecode(p:Byte Ptr) = Byte Ptr(BlueJIT.PointerToExtType)
End Type

Private
Extern
	Type Stack
		Field retIP:Byte Ptr, prevBase:Stack, varp:Long Ptr, func:Bytecode, _:Long Ptr, argv:Long Ptr, retv:Long Ptr, argc:Short, retc:Short
	End Type
	Type Bytecode
		Field mcode:Byte Ptr, idMod:Int, kcount:Int, pcount:Int, upvars:Int, frameSz:Int, icount:Int, vm:Byte Ptr
	End Type
End Extern
Public

Type BlueJIT Final
	Const PROLOGUESZ:Int = 25, ISIZE:Int = 5
	
	Global opTbl:Int(s:Stack, b:Bytecode, r:Byte Ptr)[], opc:BlueOpcode
	Global Prologue:Int[] = [ ..
		$90, $90, ..
		$8b, $44, $24, $04, ..                     'mov 4(%esp), %eax
		$89, $44, $24, $f4, ..                     'mov %eax, -12(%esp)
		$c7, $44, $24, $f8, $FF, $FF, $FF, $FF, .. 'movl ########, -8(%esp)
		$89, $64, $24, $fc, ..                     'mov %esp, -4(%esp)
		$83, $ec, $0c..                            'sub $12, %esp
	]
	
	Function Compile:Byte Ptr(vm:BlueVM, ins:Byte Ptr, icount:Int, bytecode:Bytecode)
		?Not x86
		RuntimeError "The Blue Moon JIT does not support your platform (x86-32 only at this time)"
		?
		Assert SizeOf(0:Long) = SizeOf(0:Double) And SizeOf(0:Int) = 4 And SizeOf(0:Long) = 8 And SizeOf(Byte Ptr(0)) = 4, ..
			"assumptions about platform datatype sizes are invalid"
		
		If opTbl = Null Then InitOpTbl()
		
		' compute executable code size
		Local codesize:Int = PROLOGUESZ, opPos:Int[icount]	'since sizes are irregular, looking up offsets is easiest
		For Local i:Int = 0 Until icount
			opPos[i] = codesize	'finalized below
			Select ins[i * 8]
				Case opc.SETTAB, opc.GETTAB
					codesize :+ ISIZE * 2
				Default
					codesize :+ ISIZE
			End Select
		Next
		
		' allocate executable space
		Local code:Byte Ptr = vm.mem.AllocCodeBlock(codesize)
		
		'finalize op offsets se we can look them up
		For Local i:Int = 0 Until icount
			opPos[i] :+ Int(code)	'direct to executable space
		Next
		
		' emplace prologue (used for calling in from native only)
		For Local p:Int = 0 Until PROLOGUESZ
			code[p] = Prologue[p]
		Next
		Byte Ptr Ptr(code + 14)[0] = Byte Ptr(bytecode)	'replace the ########
		
		' epilogue (shared location per-vm, probably already set)
		vm.mem.returnToNative[0] = $c30cc483	'add $12, %esp ; ret  - i.e. restore the stack to normal
		
		Local ktable:Long Ptr = Long Ptr(ins + 8 * bytecode.icount + 8 * bytecode.upvars)	'constant table
		
		' generate machine code!
		For Local i:Int = 0 Until icount	'emplace opcode calls and supporting bytecode data
			Local codep:Byte Ptr = Byte Ptr(opPos[i]), bytecodep:Byte Ptr = Byte Ptr(Int(codep) + vm.mem.PAGESZ)
			Local bi:Int = i * 8, op:Int = ins[bi], ip:Int Ptr = Int Ptr(ins + bi), func:Byte Ptr = opTbl[op]
			
			codep[0] = $e8	'call
			Byte Ptr Ptr(codep + 1)[0] = func - Int(codep + ISIZE)
			
			bytecodep[0] = ins[bi + 1] ; bytecodep[1] = ins[bi + 2]	'used by most, may get overwritten
			
			Select op
				Case opc.LOADSI, opc.CLOSURE, opc.RET, opc.POSTCALL
					Int Ptr(bytecodep + 1)[0] = ip[1]
					
				Case opc.LOADK
					Long Ptr Ptr(bytecodep + 1)[0] = ktable + ip[1]
					
				Case opc.SETTABSI, opc.GETTABSI
					Short Ptr(bytecodep)[1] = ip[1]
					
				Case opc.SETTAB, opc.GETTAB
					codep[ISIZE] = $90 ; Int Ptr(codep + ISIZE + 1)[0] = $90909090	'nops; the space is needed in the bytecode
					Long Ptr Ptr(bytecodep + 2)[0] = ktable + ip[1]
					Long Ptr Ptr(bytecodep + 2)[1] = Null	'inline cache space
					
				Case opc.GETUPV
					If Int Ptr(ins + icount * 8)[2 * bytecodep[1]] = -1
						Byte Ptr Ptr(codep + 1)[0] = Byte Ptr(BlueJIT.GETENV) - Int(codep + ISIZE)
						Byte Ptr Ptr(bytecodep + 1)[0] = Varptr(vm._ENV.val)	'note: doesn't allow _ENV to be changed from Max
					EndIf
					
				Case opc.CALL
					Short Ptr(bytecodep)[1] = ip[1]
					
				Case opc.JIF, opc.JNOT
					Int Ptr(bytecodep + 1)[0] = opPos[i + ip[1]]
					
				Case opc.JMP
					codep[0] = $e9	'use a true jump
					Int Ptr(codep + 1)[0] = opPos[i + ip[1]] - (opPos[i] + ISIZE)
					
				Default	'binary operations A = B op C
					bytecodep[2] = ip[1]
			End Select
		Next
		
		Return code
	End Function
	
	
	Const STACKFRAME_INC:Int = BlueVMMemory.STACKFRAMESZ + 4, BYTECODE_INC:Int = BlueVMMemory.BYTECODESZ + 4
	Const IP_OFFSET:Int = BlueVMMemory.PAGESZ - ISIZE
	
	
	'note: extern vars must take into account the vtbl offset, so that converting from a typed pointer can be a simple cast (faster)	
	
	Function MOV(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "MOV      //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		varp[rp[0]] = varp[rp[1]]	'assigning through Double is unsafe as it can corrupt the bit pattern
	End Function
	Function GETLC(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function SETLC(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "SETLC    //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		'add a write barrier here
		Long Ptr Ptr(varp + rp[0])[0][0] = varp[rp[1]]
	End Function
	Function LOADK(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "LOADK    //"
		Local rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET, varp:Long Ptr = stk.varp + rp[0], kp:Long Ptr = Long Ptr Ptr(rp + 1)[0]
		varp[0] = kp[0]
	End Function
	Function LOADSI(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "LOADSI   //"
		Local rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local val:Int = Int Ptr(rp + 1)[0]
		Double Ptr(stk.varp)[rp[0]] = Double(val)
	End Function
	Function LOADBOOL(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "LOADBOOL //"
		Local rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET, vp:Int Ptr = Int Ptr(stk.varp + rp[0])
		vp[1] = BlueTypeTag.NANBOX | BlueTypeTag.BOOL
		vp[0] = Int Ptr(rp + 1)[0]
	End Function
	Function LOADNIL(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "LOADNIL  //"
		Local rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Int Ptr(stk.varp + rp[0])[1] = BlueTypeTag.NANBOX | BlueTypeTag.NIL
	End Function
	
	Function GETTAB(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "GETTAB   //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET, kp:Long Ptr = Long Ptr Ptr(rp + 2)[0]
		Local tabp:Byte Ptr = Byte Ptr Ptr(varp + rp[1])[0]
		Print "  tag: " + Bin(Int Ptr(kp)[1])
		Local keyslot:Long Ptr = Null, slot:Long Ptr = BlueTable.GetSlot(tabp, kp[0], Varptr(keyslot))
		Print "  slot: " + Hex(Int(slot))
		If (slot = Null) Or (keyslot[0] <> kp[0])	'not in table; invoke metamethod
			'if keyslot == 1 then it's invalid
			DebugStop
		Else
			varp[rp[0]] = slot[0]
		EndIf
	End Function
	Function GETTABSI(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function SETTAB(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "SETTAB   //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET, kp:Long Ptr = Long Ptr Ptr(rp + 2)[0]
		Local tabp:Byte Ptr = Byte Ptr Ptr(varp + rp[0])[0]
		Print "  tag: " + Bin(Int Ptr(kp)[1])
		Local keyslot:Long Ptr = Null, slot:Long Ptr = BlueTable.GetSlot(tabp, kp[0], Varptr(keyslot))
		Print "  slot: " + Hex(Int(slot))
		If (slot = Null) Or (keyslot[0] <> kp[0])	'not in table; invoke metamethod or rawset
			'if keyslot == 1 then it's invalid
			Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(bc.vm)
			BlueTable.RawSet(vm.mem, tabp, kp[0], varp[rp[1]])
		Else
			slot[0] = varp[rp[1]]
		EndIf
	End Function
	Function SETTABSI(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "SETTABSI //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(bc.vm)
		Local tabp:Byte Ptr = varp + rp[0]
		Print "  " + Short Ptr(rp)[1]
		Local key:Long ; Double Ptr(Varptr(key))[0] = Short Ptr(rp)[1]
		BlueTable.RawSet(vm.mem, Byte Ptr Ptr(tabp)[0], key, varp[rp[1]])
	End Function
	Function GETTABI(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "GETTABI  //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local tabp:Byte Ptr = varp + rp[1]
		Print "  tag: " + Bin(Int Ptr(varp + rp[2])[1])
		Local val:Long = BlueTable.RawGet(Byte Ptr Ptr(tabp)[0], varp[rp[2]])
		Local v:Double = Double Ptr(Varptr(val))[0]
		Print "  " + v
		varp[rp[0]] = val
	End Function
	Function SETTABI(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "SETTABI  //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(bc.vm)
		Local tabp:Byte Ptr = varp + rp[0]
		Print "  " + Double Ptr(varp)[rp[2]] + " " + Double Ptr(varp)[rp[1]]
		Print "  tag: " + Bin(Int Ptr(varp + rp[2])[1])
		BlueTable.RawSet(vm.mem, Byte Ptr Ptr(tabp)[0], varp[rp[2]], varp[rp[1]])
	End Function
	Function GETUPV(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "GETUPV   //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Print "  " + rp[0] + " " + rp[1] + " " + rp[2]
		DebugStop
		Local upvp:Byte Ptr Ptr = Byte Ptr Ptr(Byte Ptr(stk) + STACKFRAME_INC), valp:Long Ptr = Long Ptr(upvp[rp[1]])
		varp[rp[0]] = valp[0]
	End Function
	Function GETENV(stk:Stack, bc:Bytecode, retptr:Byte Ptr)	'"unlisted" instruction: GETUPV for _ENV is translated into this at load-time
		Print "GETENV   //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Print "  " + rp[0] + " " + Hex(Int Ptr(rp + 1)[0])
		varp[rp[0]] = Long Ptr Ptr(rp + 1)[0][0]
	End Function
	Function SETUPV(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function NEWTAB(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "NEWTAB   //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(bc.vm)
		Local tab:Byte Ptr = vm.mem.AllocTable(Null)
		Local d:Int Ptr = Int Ptr(varp + rp[0])
		d[0] = Int(tab) ; d[1] = BlueTypeTag.NANBOX | BlueTypeTag.TBL
	End Function
	Function CLOSURE(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "CLOSURE  //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(bc.vm)
		Local d:Int Ptr = Int Ptr(varp + rp[0])
		Local cbytecode:Bytecode = vm.funIndex[Int Ptr(rp + 1)[0]], upvp:Int Ptr = Int Ptr(Byte Ptr(cbytecode) + BYTECODE_INC + 8 * cbytecode.icount)
		Local closure:Byte Ptr = vm.mem.AllocClosure(cbytecode, cbytecode.upvars)	'upvars is always even (alignment)
		
		'get upvalues off the stack
		For Local u:Int = 0 Until cbytecode.upvars
			If upvp[2 * u]
				Byte Ptr Ptr(closure)[2 + u] = Byte Ptr Ptr(Byte Ptr(stk) + STACKFRAME_INC)[upvp[2 * u + 1]]
			Else
				Byte Ptr Ptr(closure)[2 + u] = Byte Ptr Ptr(varp + upvp[2 * u + 1])[0]
			EndIf
		Next
		
		d[0] = Int(closure) ; d[1] = BlueTypeTag.NANBOX | BlueTypeTag.FUN
	End Function
	Function NEWUPV(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "NEWUPV   //"
		Local rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(bc.vm)
		Local upv:Byte Ptr = vm.mem.AllocObject(8, BlueTypeTag.UPV), d:Int Ptr = Int Ptr(stk.varp + rp[0])
		Long Ptr(upv)[0] = Long Ptr(d)[0]	'promote a value if it existed (useful for parameters)
		d[0] = Int(upv) ; d[1] = BlueTypeTag.NANBOX | BlueTypeTag.UPV
	End Function
	
	Function ADD(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		Print "ADD      //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local d:Double Ptr = Double Ptr(varp + rp[0])
		Local r:Int Ptr = Int Ptr(varp + rp[1]), l:Int Ptr = Int Ptr(varp + rp[2])
		If l[1] & BlueTypeTag.NANBOX_CHK = BlueTypeTag.NANBOX Or r[1] & BlueTypeTag.NANBOX_CHK = BlueTypeTag.NANBOX
			DebugStop
		Else
			Print "  " + Double Ptr(l)[0] + " " + Double Ptr(r)[0]
			d[0] = Double Ptr(l)[0] + Double Ptr(r)[0]
		EndIf
	End Function
	Function SUB(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function MUL(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function DIV(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function NMOD(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function POW(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function CAT(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function IDIV(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function BAND(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function BOR(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function BXOR(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function BSHL(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function BSHR(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function UNM(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function LNOT(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function ALEN(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function BNOT(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function UNP(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function EQ(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "EQ       //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = (Byte Ptr Ptr(retptr) - 4)[0] + IP_OFFSET
		Local r:Double = Double Ptr(varp)[rp[1]]
		Local l:Double = Double Ptr(varp)[rp[2]]
		Int Ptr(varp + rp[0])[0] = l = r
	End Function
	Function LT(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function LEQ(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	
	Function JMP(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
		RuntimeError "wait, why are we here?"
	End Function
	Function JIF(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "JIF      //"
		Local rp:Byte Ptr = (Byte Ptr Ptr(retptr) - 4)[0] + IP_OFFSET
		If Int Ptr(stk.varp + rp[0])[0]
			Local target:Int = Int Ptr(rp + 1)[0]
			Int Ptr(retptr)[-4] = target
		EndIf
	End Function
	Function JNOT(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function CALL(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "CALL     //"
		Local varp:Long Ptr = stk.varp, rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		Local fp:Int Ptr = Int Ptr(varp + rp[0])
		If Not PrepareCall(fp, stk, rp, varp, retptr)	'PrepareCall sets everything up so there's nothing else to do to make the call happen
			'__call metamethod
		EndIf
	End Function
	Function TCALL(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function RET:Byte Ptr(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "RET      //"
		Local varp:Long Ptr = stk.varp
		Local rp0:Byte Ptr = Byte Ptr Ptr(retptr)[-4], rp:Byte Ptr = rp0 + IP_OFFSET
		
		Local oldStk:Stack = stk.prevBase
		Local retv:Long Ptr = varp + rp[0]
		If oldStk
			oldStk.retv = retv
			oldStk.retc = Int Ptr(rp + 1)[0]
			Byte Ptr Ptr(retptr)[-4] = stk.retIP
			Byte Ptr Ptr(retptr)[-3] = Byte Ptr(oldStk)
			Byte Ptr Ptr(retptr)[-2] = Byte Ptr(oldStk.func) + BYTECODE_INC
		Else	'return to native code
			Local codePage:Byte Ptr = Byte Ptr(Int(rp0) & ((Int(2^12)-1) Shl 20))
			Byte Ptr Ptr(retptr)[-4] = codePage + 4
			Return retv
		EndIf
	End Function
	Function RETVA(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function POSTCALL(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	'	Print "POSTCALL //"
		Local varp:Long Ptr = stk.varp
		Local rp:Byte Ptr = Byte Ptr Ptr(retptr)[-4] + IP_OFFSET
		
		For Local r:Int = 0 Until Min(Int Ptr(rp + 1)[0], stk.retc)
			(varp + rp[0])[r] = stk.retv[r]
		'	Print "  return " + r + ": " + Double Ptr(stk.retv)[r]
		Next
	End Function
	Function VARARG(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function VAINIT(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	Function CALLINIT(stk:Stack, bc:Bytecode, retptr:Byte Ptr)
	End Function
	
	
	Function PrepareCall:Int(fp:Int Ptr, stk:Stack, rp:Byte Ptr, varp:Long Ptr, retptr:Byte Ptr)
		If fp[1] = BlueTypeTag.NANBOX | BlueTypeTag.FUN
			Local newStk0:Stack ; Byte Ptr Ptr(Varptr(newStk0))[0] = Byte Ptr(stk) + stk.func.frameSz
			Local newStk:Stack = newStk0	'micro-opt: newStk0 can't be a register because of the unwieldy conversion
			
			newStk.retIP = Byte Ptr Ptr(retptr)[-4]
			newStk.prevBase = stk
			Local closure:Byte Ptr = Byte Ptr(fp[0])
			Local newBC:Bytecode = Bytecode Ptr(closure)[0]
			Local voff:Int = STACKFRAME_INC + 4 * newBC.upvars
			newStk.varp = Long Ptr(Byte Ptr(newStk) + voff)
			newStk.func = newBC
			
			Local argc_actual:Int = Short Ptr(rp)[1], argc_required:Int = newBC.pcount
		'	Local argc_min:Int ; If argc_actual < argc_required Then argc_min = argc_actual Else argc_min = argc_required
			Local argc_min:Int = argc_actual - argc_required ; argc_min = (argc_min & (argc_min Shr 31)) + argc_required 'branchless 32-bit Min
			
			Local argv:Long Ptr = varp + rp[1], destv:Long Ptr = newStk.varp
			For Local a:Int = 0 Until argc_min
				destv[a] = argv[a]
			'	Print "  arg " + a + ": " + argv[a]
			Next
			For Local a:Int = argc_min Until argc_required	'nil any unfilled parameters
				Int Ptr(destv + a)[1] = BlueTypeTag.NANBOX | BlueTypeTag.NIL
			Next
			newStk.argv = argv + argc_min	'argv should be to the varargs (if any)
			newStk.argc = argc_actual - argc_min	'argc is the number of varargs
			
			Local destup:Byte Ptr Ptr = Byte Ptr Ptr(Byte Ptr(newStk) + STACKFRAME_INC)
			Local upv:Byte Ptr Ptr = Byte Ptr Ptr(closure + 8)
			For Local up:Int = 0 Until newBC.upvars	'emplace upvars
				destup[up] = upv[up]
			'	Print "  upv " + up + ": " + destv[up]
			'	Print "  val: " + Double Ptr Ptr(Byte Ptr Ptr(closure) + 2 + up)[0][0]
			Next
			
			'note that the following OVERWRITE THE PARAMETERS (in release mode), so no touching stk from here on
			Byte Ptr Ptr(retptr)[-4] = newBC.mcode
			Byte Ptr Ptr(retptr)[-3] = Byte Ptr(newStk)
			Byte Ptr Ptr(retptr)[-2] = Byte Ptr(newBC) + BYTECODE_INC
			
		ElseIf fp[1] = BlueTypeTag.NANBOX | BlueTypeTag.NATFUN	'native call
			Local fun(vm:BlueVM, ac:Int, av:Long Ptr, rv:Long Ptr) = Byte Ptr(fp[0])
			Local argc:Int = Short Ptr(rp)[1], argv:Long Ptr = varp + rp[1], retv:Long Ptr = Long Ptr(Byte Ptr(stk) + stk.func.frameSz)
			Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(stk.func.vm)
			fun(vm, argc, argv, retv)
			stk.retv = retv
		Else
			Return False	'not a function; take appropriate action
		EndIf
		Return True	'all good and will auto-call when the caller returns
	End Function
	
	Function InitOpTbl()
		opc = New BlueOpcode
		opTbl = [MOV, GETLC, SETLC, LOADK, LOADSI, LOADBOOL, LOADNIL, ..
			GETTAB, GETTABSI, SETTAB, SETTABSI, GETTABI, SETTABI, GETUPV, SETUPV, ..
			NEWTAB, CLOSURE, NEWUPV, ..
			ADD, SUB, MUL, DIV, NMOD, POW, CAT, ..
			IDIV, BAND, BOR, BXOR, BSHL, BSHR, ..
			UNM, LNOT, ALEN, BNOT, UNP, ..
			EQ, LT, LEQ, ..
			JMP, JIF, JNOT, CALL, TCALL, RETVA, RETVA, ..	'not a mistake: RET doesn't fit but we need the spacer
			POSTCALL, VARARG, VAINIT, CALLINIT]
		
		opTbl[opc.RET] = Byte Ptr(RET)
	End Function
	
	Function Identity:Byte Ptr(b:Byte Ptr)
		Return b
	End Function
	Function PointerToExtType:Byte Ptr(p:Byte Ptr)	'horrible pointer abuse (increment down so we can ignore the nonexistent vtbl)
		Return p - SizeOf(Byte Ptr(0))
	End Function
	Global BPtoS:Stack(p:Byte Ptr) = Byte Ptr(BlueJIT.PointerToExtType)
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


Rem
Local files:TList = CreateList()
Local as:String, ld:String, showHelp:Int, output:String = "a.out", doAssemble:Int = 1
Local keepAsm:Int, makeExe:Int = 1, asOpts:String, ldOpts:String, showAST:Int, showVersion:Int

?MacOS
as = "clang -m32 -c " ; ld = "clang -Wl,-no_pie -m32 -read_only_relocs suppress "
?Not MacOS
as = "gcc -m32 -c " ; ld = "gcc -m32 "
?

For Local a:Int = 1 Until AppArgs.Length
	Select AppArgs[a]
		Case "-?", "--help"
			showHelp = 1
		Case "-v"
			showVersion = 1
		Case "-o"
			If makeExe = 0
				Print "warning: -c and -o are mutually exclusive; -c is overruling -o"
			Else
				a :+ 1 ; output = AppArgs[a] ; makeExe = 2
			EndIf
		Case "-c"
			If makeExe = 2
				Print "warning: -o and -c are mutually exclusive; -o is overruling -c"
			Else
				doAssemble = 1 ; makeExe = 0
			EndIf
		Case "-s"
			keepAsm = 1
		Case "-S"
			keepAsm = 1 ; doAssemble = 0 ; makeExe = 0
		Case "--as"
			a :+ 1 ; as = AppArgs[a] + " "
		Case "--ld"
			a :+ 1 ; ld = AppArgs[a] + " "
		Case "--as-opt"
			a :+ 1 ; asOpts :+ AppArgs[a] + " "
		Case "--ld-opt"
			a :+ 1 ; ldOpts :+ AppArgs[a] + " "
		Case "--tree"
			showAST = 1 ; makeExe = 0
		Case "-w"     ; YBCodeGen.SetWarningLevel 0
		Case "--werr" ; YBCodeGen.SetWarningLevel 2
		Case "--warn" ; YBCodeGen.SetWarningLevel 1
		Default
			files.AddLast AppArgs[a]
	End Select
Next
If AppArgs.Length = 1 Then Print "ybc: no input files" ; End

If showVersion Then DisplayVersion
If showHelp Then DisplayHelp

?Win32
Local rm:String = "del /Q "
?Not Win32
Local rm:String = "rm "
?

Local allOFiles:String = ""
For Local file:String = EachIn files
	Try
		Local tree:TParseNode = YBParseFile(file)
		If showAST
			Print tree.ToString()
		Else
			YBCodeGen.Build tree
			YBAssembler.Emit file + ".s", YBCodeGen.syms, YBCodeGen.funs, YBCodeGen.vars, YBCodeGen.strs
			If doAssemble
				system_(as + asOpts + file + ".s -o " + file + ".o")
				allOFiles :+ file + ".o "
			EndIf
			If Not keepAsm
				system_ rm + file + ".s"
			EndIf
		EndIf
	Catch e:Object
		Print "Compile error:~n    " + e.ToString()
		Print "Compilation halted."
		?Debug
		Throw e
		?
		End
	End Try
Next

?Linux
Local bLib:String = "b-lib-linux"
?Not Linux
Local bLib:String = "b-lib"
?
If makeExe
	If Not FileType("b-lib.o") Then system_ as + bLib + ".s -o b-lib.o"
	system_(ld + ldOpts + "-o " + output + " " + allOFiles + " b-lib.o")
	system_(rm + allOFiles)
EndIf

Print "done."
End
End Rem
Rem
Function DisplayVersion()
	Print "Shadow SIMD Compiler: version 0.0"
End Function

Function DisplayHelp()
	Print "OVERVIEW: ybc compiler for B~n"
	Print "USAGE: ybc [options] <files>~n"
	Print "OPTIONS:~n"
	Print "  -?, --help  Display this message"
	Print "  -v          Show the compiler version"
	Print "  -o          Set the name of the output executable (default 'a.out')"
	Print "  -c          Produce separate .o files instead of an executable"
	Print "  -s          Keep text assembly .s files"
	Print "  -S          Only produce text assembly, do not assemble binaries"
	Print "  --as        Set the command to use as the assembler"
	Print "  --ld        Set the command to use as the linker"
	Print "  --as-opt    Add an option to pass to the assembler (can repeat)"
	Print "  --ld-opt    Add an option to pass to the linker (can repeat)"
	Print "  --tree      Display the AST of the program source instead of compiling"
	Print "  -w          Silence warnings"
	Print "  --werr      Convert warnings to errors"
	Print "  --warn      Notify but do not halt on warnings (default)"
End Function
End Rem
