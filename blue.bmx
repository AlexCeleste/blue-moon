
' Blue Moon
' an implementation of Lua in pure BlitzMax

SuperStrict

'Framework Brl.Blitz
'Import Brl.LinkedList
'Import Brl.Map
'Import Brl.Reflection

Import "bluecompiler.bmx"
Import "blueallocator.bmx"

'Const file:String = "tests/checktable.lua"
'Const file:String = "tests/mandelbrot.lua"
'Const file:String = "tests/sieve.lua"
'Const file:String = "tests/attrib.lua"
Const file:String = "test.lua"

Const outFile:String = "out.lua.so"


Print BlueCompiler.ShowBytecode(file)

Local code:BlueBinary = BlueCompiler.CompileFileForLoad(file)
Local vm:BlueVM = New BlueVM
Local tl:BlueLuaVal = vm.LoadObjectCode(code)
'tl.Call()

Local stk:Stack = BlueJIT.BPtoS(vm.mem.stack)
stk.retIP = Null ; stk.prevBase = Null
Local vc:Int = 10, upvars:Int = 1
stk.varp = Double Ptr(Byte Ptr(stk) + BlueJIT.STACKFRAME_INC) + upvars
stk.func = vm.funIndex[0]
stk.IP = 0
stk.argv = Null	'may want to add space?
stk.retv = Null
stk.argc = 0
stk.retc = 0

Print "running..."
Local t:Int = MilliSecs()
Local test:Int(_:Byte Ptr) = stk.func.mcode - BlueJIT.PROLOGUESZ ; test(stk)
t = MilliSecs() - t
Print t
Print "run complete"


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
	
	Method New()
		mem = New BlueVMMemory
	End Method
	
	' Load the procedures and constants of a compiled binary into the VM, returning the function representing the program toplevel
	Method LoadObjectCode:BlueLuaVal(code:BlueBinary)
		Local buf:Int[] = code.buf
		Local fcount:Int = buf[0], kcount:Int = buf[1], ktblpos:Int = buf[2], ktbl:Double[kcount]
		Local convert:Byte Ptr(o:Object) = Byte Ptr(BlueJIT.Identity)
		
		For Local k:Int = 0 Until kcount
			Local koff:Int = buf[ktblpos + k], sz:Int = buf[ktblpos + k + 1] - koff
			If sz > 2	'strings have size > 2
				Local length:Double, lp:Int Ptr = Int Ptr(Varptr(length))
				lp[0] = buf[koff] ; lp[1] = buf[koff + 1]
				Local s:Byte Ptr = mem.AllocObjectOldStr(Ceil(length / 2) * 4, BlueTypeTag.STR)
				For Local c:Int = 0 Until Ceil(length / 2)
					Int Ptr(s)[c] = buf[koff + 2 + c]
				Next
				ktbl[k] = mem.PtrToVal(s, BlueTypeTag.STR)
			Else
				Local d:Double, dp:Int Ptr = Int Ptr(Varptr(d))
				dp[0] = buf[koff] ; dp[1] = buf[koff + 1]
				ktbl[k] = d
			EndIf
		Next
		
		ExtendFunIndex(idMod + fcount)
		For Local f:Int = 0 Until fcount
			Local foff:Int = buf[3 + f]
			Local ic:Int = buf[foff + 1], kc:Int = buf[foff + 2], pc:Int = buf[foff + 3], uc:Int = buf[foff + 5], fsz:Int = buf[foff + 6]
			
			Local b:Bytecode = BPtoBC(mem.AllocObjectOldStr((8 + 2 * uc + 2 * kc + 2 * ic) * 4, BlueTypeTag.HEAVY))
			b.idMod = idMod ; b.kcount = kc ; b.pcount = pc ; b.upvars = uc
			b.frameSz = BlueJIT.STACKFRAMESZ + b.upvars * 8 + fsz * 8 ; b.icount = ic ; b.vm = convert(Self)
			
			Local ib:Int Ptr = Int Ptr(Byte Ptr(b) + 4) + 8 + 2 * ic
			For Local u:Int = 0 Until uc
				Local uoff:Int = 7 + u * 2
				ib[u * 2] = buf[foff + uoff] ; ib[u * 2 + 1] = buf[foff + uoff + 1]
			Next
			
			Local db:Double Ptr = Double Ptr(ib + 2 * uc)
			For Local k:Int = 0 Until kc
				db[k] = ktbl[buf[foff + 7 + uc * 2 + k]]
			Next
			
			Local ioff:Int = 7 + 2 * uc + kc ; ib = Int Ptr(Byte Ptr(b) + 4) + 8
			
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
	
	Global BPtoBC:Bytecode(p:Byte Ptr) = Byte Ptr(BlueJIT.PointerToExtType)
End Type

Private
Extern
	Type Stack
		Field retIP:Byte Ptr, prevBase:Stack, varp:Double Ptr, func:Bytecode, IP:Int, argv:Byte Ptr, retv:Byte Ptr, argc:Short, retc:Short
	End Type
	Type Bytecode
		Field mcode:Byte Ptr, idMod:Int, kcount:Int, pcount:Int, upvars:Int, frameSz:Int, icount:Int, vm:Byte Ptr
	End Type
End Extern
Public

Type BlueJIT Final
	Const BLOCKSZ:Int = BlueVMMemory.FUNCSIZE, PROLOGUESZ:Int = 25, ISIZE:Int = 5
	
	Global opTbl:Int(s:Stack, b:Byte Ptr)[], opc:BlueOpcode
	Global Prologue:Int[] = [ ..
		$90, $90, ..
		$8b, $44, $24, $04, ..                     'mov 4(%esp), %eax
		$89, $44, $24, $f4, ..                     'mov %eax, -12(%esp)
		$c7, $44, $24, $f8, $FF, $FF, $FF, $FF, .. 'movl ########, -8(%esp)
		$89, $64, $24, $fc, ..                     'mov %esp, -4(%esp)
		$83, $ec, $0c..                            'sub $12, %esp
	]
	Global trueReturn:Byte Ptr
	
	Function Compile:Byte Ptr(vm:BlueVM, ins:Byte Ptr, icount:Int, bytecode:Bytecode)
		?Not x86
		RuntimeError "The Blue Moon JIT does not support your platform (x86-32 only at this time)"
		?
		
		If opTbl = Null Then InitOpTbl()
		
		Local sz:Int = PROLOGUESZ + icount * ISIZE
		sz :+ (sz / BLOCKSZ) * ISIZE
		
		' allocate executable space
		Local blockCount:Int = Ceil(sz / Double(BLOCKSZ)), block:Byte Ptr[blockCount]
		For Local b:Int = 0 Until blockCount
			block[b] = vm.mem.AllocCodeBlock()
		Next
		
		For Local p:Int = 0 Until PROLOGUESZ	'emplace prologue (used for calling in from native only)
			block[0][p] = Prologue[p]
		Next
		Byte Ptr Ptr(block[0] + 14)[0] = ins	'replace the ########
		
		' generate machine code!
		Local pos:Int = PROLOGUESZ, sizedec:Int = icount + PROLOGUESZ / ISIZE
		For Local b:Int = 0 Until block.Length
			For Local i:Int = 0 Until BLOCKSZ - ISIZE Step ISIZE	'emplace opcode calls
				If sizedec = 0 Then Exit
				If sizedec <= icount
					block[b][i] = $e8
					Local func:Byte Ptr = opTbl[ins[(icount - sizedec) * 8]]
					Byte Ptr Ptr(block[b] + i + 1)[0] = func - Int(block[b] + i + ISIZE)
				EndIf
				sizedec :- 1
			Next
			If b < block.Length - 1	'emplace jumps between blocks
				block[b][BLOCKSZ - ISIZE] = $e9
				Byte Ptr Ptr(block[b] + (BLOCKSZ - ISIZE) + 1)[0] = (block[b] + BLOCKSZ) - Int(block[b + 1])
			EndIf
		Next
		If sz Mod BLOCKSZ = 0	'if necessary add the final instruction
			Local blkp:Byte Ptr = block[block.Length - 1] + (BLOCKSZ - ISIZE)
			blkp[0] = $e8
			Local func:Byte Ptr = opTbl[ins[(icount - 1) * 8]]
			Byte Ptr Ptr(blkp + 1)[0] = func - Int(blkp + ISIZE)
		EndIf
		
		'replace offsets in the bytecode with actual targets while we have the blocks together
		For Local i:Int = 0 Until icount
			Local ip:Int Ptr = Int Ptr(ins + i * 8)
			Select ins[i * 8]
				Case opc.JMP, opc.JIF, opc.JNOT		'jumps can use the machine code pointers
					Local tgt:Int = i + ip[1], tgtofs:Int = tgt * ISIZE + PROLOGUESZ
					Local tgtblk:Byte Ptr = block[tgtofs / (BLOCKSZ - ISIZE)], dst:Byte Ptr = tgtblk + tgtofs Mod (BLOCKSZ - ISIZE)
					
					If ins[i * 8] = opc.JMP 'need to provide both the machine code jump and the bytecode jump
						ip[0] = Int(dst)		'JMP takes no operands which is handy
						ip[1] :* IP_INCR
					Else
						Local disp:Int = ip[1], r:Int = Byte Ptr(ip)[1]
						ip[1] = Int(dst)		'JIF/JNOT need a byte preserved for the operand register
						ip[0] = ((disp * IP_INCR) Shl 8) | (r & $FF)	' using the lowest one lets us sign-extend the disp. more easily
					EndIf
					
				Case opc.LOADK	'get the direct addresses of k-table slots
					Local kp:Double Ptr = Double Ptr(ins + 8 * bytecode.icount + 8 * bytecode.upvars) + ip[1]
					ip[1] = Int(kp)
			End Select
		Next
		
		trueReturn = Byte Ptr((Int(block[0]) & ((Int(2^12)-1) Shl 20)) + vm.mem.PAGESZ - 4)	'sneak it in the spare rwx space
		Int Ptr(trueReturn)[0] = $c30cc483	'add $12, %esp ; ret  - i.e. restore the stack to normal
		
		Return block[0]
	End Function
	
	
	Const STACKFRAMESZ:Int = 8 * 4, BYTECODESZ:Int = 8 * 4, STACKFRAME_INC:Int = STACKFRAMESZ + 4, BYTECODE_INC:Int = BYTECODESZ + 4
	Const IP_INCR:Int = 8
	
		
	Function MOV(stk:Stack, bytecode:Byte Ptr)
	'	Print "MOV      // ip:  " + stk.IP
		'note (here and below); vars must take into account the vtbl offset, so that converting from a typed pointer can be a simple cast (faster)
		Local varp:Double Ptr = stk.varp, ins:Byte Ptr = (bytecode + stk.IP)
		varp[ins[1]] = varp[ins[2]]
		stk.IP :+ IP_INCR
	End Function
	Function GETLC(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function SETLC(stk:Stack, bytecode:Byte Ptr)
		Print "SETLC    // ip:  " + stk.IP
		Local varp:Double Ptr = stk.varp
		Local ins:Byte Ptr = (bytecode + stk.IP)
		Double Ptr Ptr(varp + ins[1])[0][0] = varp[ins[2]]
		stk.IP :+ IP_INCR
	End Function
	Function LOADK(stk:Stack, bytecode:Byte Ptr)
	'	Print "LOADK    // ip:  " + stk.IP
		Local ins:Byte Ptr = (bytecode + stk.IP), kp:Double Ptr = Double Ptr Ptr(ins)[1]
		stk.varp[ins[1]] = kp[0]
		stk.IP :+ IP_INCR
	End Function
	Function LOADSI(stk:Stack, bytecode:Byte Ptr)
	'	Print "LOADSI   // ip:  " + stk.IP
		Local ins:Byte Ptr = (bytecode + stk.IP), val:Int = Int Ptr(ins)[1]
		stk.varp[ins[1]] = Double(val)
		stk.IP :+ IP_INCR
	End Function
	Function LOADBOOL(stk:Stack, bytecode:Byte Ptr)
	'	Print "LOADBOOL // ip:  " + stk.IP
		Local ins:Byte Ptr = (bytecode + stk.IP), vp:Int Ptr = Int Ptr(stk.varp + ins[1])
		vp[1] = BlueTypeTag.NANBOX | BlueTypeTag.BOOL
		vp[0] = Int Ptr(ins)[1]
		stk.IP :+ IP_INCR
	End Function
	Function LOADNIL(stk:Stack, bytecode:Byte Ptr)
	'	Print "LOADNIL  // ip:  " + stk.IP
		Local ins:Byte Ptr = (bytecode + stk.IP)
		Int Ptr(stk.varp + ins[1])[1] = BlueTypeTag.NANBOX | BlueTypeTag.NIL
		stk.IP :+ IP_INCR
	End Function
	
	Function GETTAB(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function GETTABSI(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function SETTAB(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function SETTABSI(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function GETTABI(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function SETTABI(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function GETUPV(stk:Stack, bytecode:Byte Ptr)
	'	Print "GETUPV   // ip:  " + stk.IP
		Local varp:Double Ptr = stk.varp, upvp:Double Ptr = Double Ptr(Byte Ptr(stk) + STACKFRAME_INC)
		Local ins:Byte Ptr = (bytecode + stk.IP), valp:Double Ptr = Double Ptr Ptr(upvp + ins[2])[0]
		varp[ins[1]] = valp[0]
		stk.IP :+ IP_INCR
	End Function
	Function SETUPV(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function NEWTAB(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function CLOSURE(stk:Stack, bytecode:Byte Ptr)
	'	Print "CLOSURE  // ip:  " + stk.IP
		Local varp:Double Ptr = stk.varp, ins:Byte Ptr = (bytecode + stk.IP)
		Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(Byte Ptr Ptr(bytecode)[-1])
		Local d:Int Ptr = Int Ptr(varp + ins[1])
		Local cbytecode:Bytecode = vm.funIndex[Int Ptr(ins)[1]], upvp:Int Ptr = Int Ptr(Byte Ptr(cbytecode) + BYTECODE_INC + 8 * cbytecode.icount)
		Local closure:Byte Ptr = vm.mem.AllocObject(8 + cbytecode.upvars * 8, BlueTypeTag.FUN)
		
		'get upvalues off the stack
		For Local u:Int = 0 Until cbytecode.upvars
			If upvp[2 * u]
				Double Ptr(closure)[1 + u] = Double Ptr(Byte Ptr(stk) + STACKFRAME_INC)[upvp[2 * u + 1]]
			Else
				Double Ptr(closure)[1 + u] = varp[upvp[2 * u + 1]]
			EndIf
		Next
		Byte Ptr Ptr(closure)[0] = Byte Ptr(cbytecode)
		
		d[0] = Int(closure) ; d[1] = BlueTypeTag.NANBOX | BlueTypeTag.FUN
		stk.IP :+ IP_INCR
	End Function
	Function NEWUPV(stk:Stack, bytecode:Byte Ptr)
	'	Print "NEWUPV   // ip:  " + stk.IP
		Local ins:Byte Ptr = (bytecode + stk.IP)
		Local convert:BlueVM(p:Byte Ptr) = Byte Ptr(Identity), vm:BlueVM = convert(Byte Ptr Ptr(bytecode)[-1])
		Local upv:Byte Ptr = vm.mem.AllocObject(8, BlueTypeTag.UPV), d:Int Ptr = Int Ptr(stk.varp + ins[1])
		d[0] = Int(upv) ; d[1] = BlueTypeTag.NANBOX | BlueTypeTag.UPV
		stk.IP :+ IP_INCR
	End Function
	
	Function ADD(stk:Stack, bytecode:Byte Ptr)
		Print "ADD      // ip:  " + stk.IP
		Local varp:Double Ptr = stk.varp, ins:Byte Ptr = (bytecode + stk.IP)
		
		Local d:Double Ptr = varp + ins[1]
		Local r:Int Ptr = Int Ptr(varp + ins[2])
		Local l:Int Ptr = Int Ptr(varp + Int Ptr(ins)[1])
		If l[1] & BlueTypeTag.NANBOX = BlueTypeTag.NANBOX Or r[1] & BlueTypeTag.NANBOX = BlueTypeTag.NANBOX
			DebugStop
		Else
			d[0] = Double Ptr(l)[0] + Double Ptr(r)[0]
			stk.IP :+ IP_INCR
		EndIf
	End Function
	Function SUB(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function MUL(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function DIV(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function NMOD(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function POW(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function CAT(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function IDIV(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function BAND(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function BOR(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function BXOR(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function BSHL(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function BSHR(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function UNM(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function LNOT(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function ALEN(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function BNOT(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function UNP(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function EQ(stk:Stack, bytecode:Byte Ptr)
	'	Print "EQ       // ip:  " + stk.IP
		Local varp:Double Ptr = stk.varp, ins:Byte Ptr = (bytecode + stk.IP)
		Local r:Double = varp[ins[2]]
		Local l:Double = varp[Int Ptr(ins)[1]]
		varp[ins[1]] = l = r
		stk.IP :+ IP_INCR
	End Function
	Function LT(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function LEQ(stk:Stack, bytecode:Byte Ptr)
	End Function
	
	Function JMP(stk:Stack, bytecode:Byte Ptr, retptr:Byte Ptr)
	'	Print "JMP      // ip:  " + stk.IP
		Local target:Int = Int Ptr(bytecode + stk.IP)[0]
		Local disp:Int = Int Ptr(bytecode + stk.IP)[1]
		stk.IP :+ disp
		Int Ptr(retptr)[-4] = target
	End Function
	Function JIF(stk:Stack, bytecode:Byte Ptr, retptr:Byte Ptr)
	'	Print "JIF      // ip:  " + stk.IP
		Local ins:Byte Ptr = (bytecode + stk.IP)
		If stk.varp[ins[0]] <> 0
			Local target:Int = Int Ptr(ins)[1]
			Local disp:Int = Int Ptr(ins)[0] Sar 8
			stk.IP :+ disp
			Int Ptr(retptr)[-4] = target
		Else
			stk.IP :+ IP_INCR
		EndIf
	End Function
	Function JNOT(stk:Stack, bytecode:Byte Ptr, retptr:Byte Ptr)
	End Function
	Function CALL(stk:Stack, bytecode:Byte Ptr, retptr:Byte Ptr)
		Print "CALL     // ip:  " + stk.IP
		Local varp:Double Ptr = stk.varp, ins:Byte Ptr = (bytecode + stk.IP)
		Local fp:Int Ptr = Int Ptr(varp + ins[1])
		If Not PrepareCall(fp, stk, ins, varp, retptr)	'PrepareCall sets everything up so there's nothing else to do to make the call happen
			'__call metamethod
		EndIf
	End Function
	Function TCALL(stk:Stack, bytecode:Byte Ptr, retptr:Byte Ptr)
	End Function
	Function RET:Byte Ptr(stk:Stack, bytecode:Byte Ptr, retptr:Byte Ptr)
	'	Print "RET      // ip:  " + stk.IP
		Local varp:Double Ptr = stk.varp'Byte Ptr(stk) + vars
		Local ins:Byte Ptr = (bytecode + stk.IP)
		
		Local oldStk:Stack = stk.prevBase
		Local retv:Byte Ptr = varp + ins[1]
		If oldStk
			oldStk.retv = retv
			oldStk.retc = Int Ptr(ins)[1]
			Byte Ptr Ptr(retptr)[-4] = stk.retIP
			Byte Ptr Ptr(retptr)[-3] = Byte Ptr(oldStk)
			Byte Ptr Ptr(retptr)[-2] = Byte Ptr(oldStk.func) + BYTECODE_INC
		Else	'return to native code
			Byte Ptr Ptr(retptr)[-4] = truereturn
			Return retv
		EndIf
	End Function
	Function RETVA(stk:Stack, bytecode:Byte Ptr, retptr:Byte Ptr)
	End Function
	Function POSTCALL(stk:Stack, bytecode:Byte Ptr)
		Print "POSTCALL // ip:  " + stk.IP
		Local varp:Byte Ptr = stk.varp'Byte Ptr(stk) + vars
		Local ins:Byte Ptr = (bytecode + stk.IP)
		
		For Local r:Int = 0 Until Min(Int Ptr(ins)[1], stk.retc)
			(Double Ptr(varp) + ins[1])[r] = Double Ptr(stk.retv)[r]
			Print "  return " + r + ": " + Double Ptr(stk.retv)[r]
		Next
		
		stk.IP :+ IP_INCR
	End Function
	Function VARARG(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function VAINIT(stk:Stack, bytecode:Byte Ptr)
	End Function
	Function CALLINIT(stk:Stack, bytecode:Byte Ptr)
	End Function
	
	
	Function PrepareCall:Int(fp:Int Ptr, stk:Stack, ins:Byte Ptr, varp:Double Ptr, retptr:Byte Ptr)
		Local tag:Int = fp[1] & (BlueTypeTag.NANBOX | BlueTypeTag.NATFUN)
		If tag = BlueTypeTag.NANBOX | BlueTypeTag.FUN
			Local newStk0:Stack ; Byte Ptr Ptr(Varptr(newStk0))[0] = Byte Ptr(stk) + stk.func.frameSz
			Local newStk:Stack = newStk0	'micro-opt: newStk0 can't be a register because of the unwieldy conversion
			
			newStk.retIP = Byte Ptr Ptr(retptr)[-4]
			newStk.prevBase = stk
			Local closure:Byte Ptr = Byte Ptr(fp[0])
			Local newBC:Bytecode = Bytecode Ptr(closure)[0]
			Local voff:Int = STACKFRAME_INC + 8 * newBC.upvars' + 4
			newStk.varp = Double Ptr(Byte Ptr(newStk) + voff)
			newStk.func = newBC
			newStk.IP = 0
			
			Local argc_actual:Int = Int Ptr(ins)[1], argc_required:Int = newBC.pcount
		'	Local argc_min:Int ; If argc_actual < argc_required Then argc_min = argc_actual Else argc_min = argc_required
			Local argc_min:Int = argc_actual - argc_required ; argc_min = (argc_min & (argc_min Shr 31)) + argc_required 'branchless 32-bit Min
			Local argv:Double Ptr = varp + ins[2], destv:Double Ptr = Double Ptr(Byte Ptr(newStk) + voff)
			For Local a:Int = 0 Until argc_min
				destv[a] = argv[a]
				Print "  arg " + a + ": " + argv[a]
			Next
			For Local a:Int = argc_min Until argc_required	'nil any unfilled parameters
				Int Ptr(destv + a)[1] = BlueTypeTag.NANBOX | BlueTypeTag.NIL
			Next
			newStk.argv = argv + argc_min	'argv should be to the varargs (if any)
			newStk.argc = argc_actual - argc_min	'argc is the number of varargs
			destv = Double Ptr(Byte Ptr(newStk) + STACKFRAME_INC)
			Local upv:Double Ptr = Double Ptr(closure + 8)'Byte Ptr(newBC) + BYTECODE_INC + newBC.icount * 8)
			For Local up:Int = 0 Until newBC.upvars	'emplace upvars
				destv[up] = upv[up]
				Print "  upv " + up + ": " + destv[up]
				Print "  val: " + Double Ptr Ptr(Double Ptr(closure) + 1 + up)[0][0]
			Next
			
			stk.IP :+ IP_INCR	'note that the following OVERWRITE THE PARAMETERS (in release mode), so no touching stk from here on
			Byte Ptr Ptr(retptr)[-4] = newBC.mcode' + BlueJIT.PROLOGUESZ
			Byte Ptr Ptr(retptr)[-3] = Byte Ptr(newStk)
			Byte Ptr Ptr(retptr)[-2] = Byte Ptr(newBC) + BYTECODE_INC
			
		ElseIf tag = BlueTypeTag.NANBOX | BlueTypeTag.NATFUN
			'native call
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
			EQ, LT, LEQ]
		Local ops1:Int(s:Stack, b:Byte Ptr, r:Byte Ptr)[] = [JMP, JIF, JNOT, CALL, TCALL]', RET, RETVA]
		Local ops2:Int(s:Stack, b:Byte Ptr)[] =  [POSTCALL, VARARG, VAINIT, CALLINIT]
		opTbl = opTbl[..50]
		For Local op:Int = opc.JMP To opc.TCALL'RETVA		'this is a nasty way to do things
			opTbl[op] = Byte Ptr(ops1[op - opc.JMP])
		Next
		opTbl[opc.RET] = Byte Ptr(RET) ; opTbl[opc.RETVA] = Byte Ptr(RETVA)
		For Local op:Int = opc.POSTCALL To opc.CALLINIT
			opTbl[op] = ops2[op - opc.POSTCALL]
		Next
	End Function
	
	Function Identity:Byte Ptr(b:Byte Ptr)
		Return b
	End Function
	Function PointerToExtType:Byte Ptr(p:Byte Ptr)	'horrible pointer abuse (increment down so we can ignore the nonexistent vtbl)
		Return p - 4'sizeof(Byte Ptr)
	End Function
	Global BPtoS:Stack(p:Byte Ptr) = Byte Ptr(BlueJIT.PointerToExtType)
End Type

Type BlueLuaVal
	Field _obj:BlueGCNode

	Method Call()
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
