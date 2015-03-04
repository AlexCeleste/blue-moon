
' Blue Moon
' Bytecode compiler: accepts program fragments as strings, or program files (and outputs)

SuperStrict

Import "blueparser.bmx"
Import "blueerror.bmx"
Import "blueversion.bmx"
Private
Include "TFold.bmx"
Include "bluefoldsyntax.bmx"
Include "bluefoldsemantics.bmx"
Include "bluefoldcodegen.bmx"
Include "debugfold.bmx"
Public

Type BlueCompiler
	Function CompileFile(infile:String, outfile:String = "")
		If outfile = "" Then outfile = infile + ".so"
		Local n:TParseNode = BlueParseFile(infile)
		Local compiled:BlueBinary = BlueBinary.Make(infile, ASTToAssembly(n))
		compiled.WriteObjectFile(outFile)
	End Function
	
	Function CompileFileForLoad:BlueBinary(infile:String)
		Local n:TParseNode = BlueParseFile(infile)
		Local code:BlueBinary = BlueBinary.Make(infile, ASTToAssembly(n))
		Local sz:Int, mem:Byte Ptr = code.WriteMemory(sz)
		code = code.ReadMemory(mem, sz) ; MemFree(mem)
		Return code
	End Function
	
'	Function CompileString:BlueBinary(program:String)
'		Local n:TParseNode = BlueParseString(program)
'		Return BlueBinary.Make("<string>", ASTToAssembly(n))
'	End Function
	
	Function ShowBytecode:String(infile:String)
		Local n:TParseNode = BlueParseFile(infile), out:String = infile + ":~n"
		Local assemblies:TList = ASTToAssembly(n)
		For Local f:BlueAssembly = EachIn assemblies
			out :+ f.ToString()
		Next
		Return out
	End Function
	
	Function ASTToAssembly:TList(n:TParseNode)
		Local tbl:NodeTable = Node.FromParse(n), r:Node = tbl.n
	'	Print "AST: " + r.ToString()
		
		r = Node.Fold(r, NormalizeFold.Make(tbl))
	'	Print "Normalized: " + r.ToString()
		
		Local errf:SyntaxErrFold = SyntaxErrFold.Make(tbl)
		r = Node.Fold(r, errf) ; errf.BreakCheck()
		
		r = Node.Fold(r, New ConstantFold)
		
		Local semf:SemanticFold = SemanticFold.Make(r, tbl)
		r = Node.Fold(r, semf, Node.BOTH)	'fold both ways to push and pop environments
		SemanticFold.Close(semf, r)
	'	Print "Semantic'd: " + r.ToString()		
		Local funs:FunDef[] = FunDef[](semf.funs.ToArray()) ; semf = Null
		
		' if we wanted a type checker, it would go here
		tbl = Null	'source information no longer needed
		
		Local assemblies:TList = CreateList()
		For Local f:FunDef = EachIn funs
			Local com:CompileFold = CompileFold.Make(BlueAssembly.Make(f))
		'	Print "F #" + f.id + ": " + f.n.ToString()
			Local inst:TList = BytecodeNode(Node.Fold(f.n, com)).inst
			com.f.Finish(inst, com.opc)
			assemblies.AddLast(com.f)
		Next
		
		?Debug
		Debug_KeyListFold.Run(funs, CompileFold.Make(Null))
		?
		Return assemblies
	End Function
End Type

Type BlueOpcode Final
	Field MOV:Int = 0, GETLC:Int = 1, SETLC:Int = 2		'these are constants, ignore their definition as fields
	Field LOADK:Int = 3, LOADSI:Int = 4, LOADBOOL:Int = 5, LOADNIL:Int = 6
	
	Field GETTAB:Int = 7, GETTABSI:Int = 8, SETTAB:Int = 9, SETTABSI:Int = 10
	Field GETTABI:Int = 11, SETTABI:Int = 12, GETUPV:Int = 13, SETUPV:Int = 14
	Field NEWTAB:Int = 15, CLOSURE:Int = 16, NEWUPV:Int = 17
	
	Field ADD:Int = 18, SUB:Int = 19, MUL:Int = 20, DIV:Int = 21, NMOD:Int = 22, POW:Int = 23, CAT:Int = 24
	Field IDIV:Int = 25, BAND:Int = 26, BOR:Int = 27, BXOR:Int = 28, BSHL:Int = 29, BSHR:Int = 30
	Field UNM:Int = 31, LNOT:Int = 32, ALEN:Int = 33, BNOT:Int = 34, UNP:Int = 35
	Field EQ:Int = 36, LT:Int = 37, LEQ:Int = 38
	
	Field JMP:Int = 39, JIF:Int = 40, JNOT:Int = 41
	Field CALL:Int = 42, TCALL:Int = 43, RET:Int = 44, RETVA:Int = 45
	Field POSTCALL:Int = 46, VARARG:Int = 47, VAINIT:Int = 48, CALLINIT:Int = 49
	
	Const NUM_OPCODES:Int = 50
	
	Function Make:Int(i:BlueInstr)
		Global opc:BlueOpcode = New Self
		Local c:Int = i.c
		Select i.op
			Case opc.LOADK, opc.LOADSI, opc.LOADBOOL, opc.LOADNIL, opc.JMP, opc.JIF, opc.JNOT, opc.RET
				If c < 0 Then c :& (Int(2^18) - 1)
				Return i.op | ((i.ra & $FF) Shl 6) | (c Shl 14)
			Default
				If c < 0 Then c :& (Int(2^10) - 1)
				Return i.op | ((i.ra & $FF) Shl 6) | ((i.rb & $FF) Shl 14) | (c Shl 22)
		End Select
	End Function
	Function Read(i:Int, op:Int Var, ra:Int Var, rb:Int Var, c:Int Var)
		Global opc:BlueOpcode = New Self
		op = i & %111111 ; ra = (i Shr 6) & $FF
		Select op
			Case opc.LOADK, opc.LOADSI, opc.LOADBOOL, opc.LOADNIL, opc.JMP, opc.JIF, opc.JNOT, opc.RET
				rb = 0 ; c = i Sar 14	'sign-extend (already MSB)
			Default
				rb = (i Shr 14) & $FF ; c = i Sar 22
		End Select
	End Function
End Type

Type BlueInstr
	Field op:Int, ra:Int, rb:Int, c:Int
	Function Make:BlueInstr(op:Int, ra:Int, rb:Int, c:Int = 0)
		Local i:BlueInstr = New BlueInstr ; i.op = op ; i.ra = ra ; i.rb = rb ; i.c = c ; Return i
	End Function
	Function MakeK:BlueInstr(op:Int, ra:Int, c:Int)
		Return Make(op, ra, 0, c)
	End Function
	Method ToString:String()
		Global opc:BlueOpcode = New BlueOpcode
		Local n:String = "??"
		For Local f:TField = EachIn TTypeId.ForObject(opc).Fields()
			If f.GetInt(opc) = op Then n = f.Name()[..10] ; Exit
		Next
		Select op
			Case opc.LOADNIL, opc.NEWUPV, opc.NEWTAB
				Return n + " " + ra
			Case opc.MOV, opc.SETLC, opc.GETLC, opc.SETUPV, opc.GETUPV
				Return n + " " + ra + ", " + rb
			Case opc.LOADK, opc.LOADSI, opc.LOADBOOL, opc.CLOSURE, opc.RET, opc.POSTCALL, opc.VARARG
				Return n + " " + ra + ", " + c
			Default ; Return n + " " + ra + ", " + rb + ", " + c
		End Select
	End Method
End Type

Type BlueBinary
	Const MAGICNUMBER:Long = 5642816051412814914:Long	'BLUEMOON
	Field out:Int[], buf:Int[]	'out = output bytestream, buf = input/readable bytestream
	
	Function Make:BlueBinary(file:String, funs:TList)
		Local b:BlueBinary = New Self, ktbl:TList = CreateList(), kc:Int = 0
		
		Local binSz:Int = 0, fcount:Int = 0
		For Local f:BlueAssembly = EachIn funs
			Local ki:Int = 0
			For Local k:String = EachIn f.ktbl
				f.kidx[ki] = AddKTblEntry(k, ktbl, kc) ; ki :+ 1	'consolidate constants into single table
			Next
			
			If f.framesz > f.MAXVARS - 3 Or f.upvars > f.MAXVARS Then Throw ..
				BlueAssemblyError.Make(file, "too many variables/temporaries in function #" + f.id ..
				+ "; consider splitting into more blocks or helper functions")
			
			binSz :+ (7 ..	'id, instruction count, k count, param count, var count, upvar count, frame size
			       + 2 * f.upvars ..	'upvar levels and stack positions (interleaved)
			       + f.kc ..	'constant index table
			       + f.inst.Count())	'instructions
			fcount :+ 1
		Next
		
		binSz :+ (kc + 1)
		For Local k:String = EachIn ktbl
			If k[0] = "0"[0] Then binSz :+ 2 Else binSz :+ Ceil((k.Length - 2) / 2.0) + 2
		Next
		
		Local headerSz:Int = 3 + fcount	'function count, k count, k table offset, function offset list
		binSz :+ headerSz
		
		Local out:Int[] = New Int[binSz] ; b.out = out
		out[0] = fcount
		out[1] = kc
		
		Local pos:Int = headerSz, opc:BlueOpcode = New BlueOpcode
		For Local f:BlueAssembly = EachIn funs
			out[3 + f.id] = pos
			out[pos] = f.id ; out[pos + 1] = f.inst.Count() ; out[pos + 2] = f.kc ; out[pos + 3] = f.params
			out[pos + 4] = f.vars ; out[pos + 5] = f.upvars ; out[pos + 6] = f.framesz
			pos :+ 7
			For Local v:Int = 0 Until f.upvars
				out[pos] = f.upvarTier[v] ; out[pos + 1] = f.upvarStkPos[v] ; pos :+ 2
			Next
			For Local k:Int = 0 Until f.kc
				out[pos] = f.kidx[k] ; pos :+ 1
			Next
			For Local i:BlueInstr = EachIn f.inst
				out[pos] = BlueOpcode.Make(i) ; pos :+ 1
			Next
		Next
		
		out[2] = pos ; Local ktblpos:Int = pos ; pos :+ (kc + 1)
		For Local k:String = EachIn ktbl
			Local d:Double, dp:Byte Ptr = Varptr(d)
			out[ktblpos] = pos ; ktblpos :+ 1
			If k[0] = "0"[0]
				d = Double(k[2..])
				out[pos] = Int Ptr(dp)[0] ; out[pos + 1] = Int Ptr(dp)[1]
				pos :+ 2
			Else
				d = k.Length - 2
				out[pos] = Int Ptr(dp)[0] ; out[pos + 1] = Int Ptr(dp)[1] ; pos :+ 2
				If k.Length Mod 2 = 1 Then k :+ " "	'even the length for ease of writing
				For Local c:Int = 2 Until k.Length Step 2
					out[pos] = Short(k[c]) | (Short(k[c + 1]) Shl 16) ; pos :+ 1
				Next
			EndIf
		Next
		out[ktblpos] = pos
		Assert pos = binSz, "assembly write error: binary size mismatch (critical compiler error)"
		
		Return b
	End Function
	
	Function AddKTblEntry:Int(k:String, tbl:TList, kc:Int Var)
		Local ki:Int = 0
		For Local ek:String = EachIn tbl
			If ek = k Then Return ki
			ki :+ 1
		Next
		tbl.AddLast(k) ; kc :+ 1'ktbl.Count()
		Return ki
	End Function
	
	Method WriteMemory:Byte Ptr(size:Int Var)
		size = out.Length * 8 + 16
		Local mem:Byte Ptr = MemAlloc(size)
		Assert mem, "failed to allocate memory for instruction buffer"
		Write(CreateRamStream(mem, out.Length * 8 + 16, 0, 1))
		Return mem
	End Method
	
	Method WriteObjectFile(filename:String)
		Write(WriteFile(filename))
	End Method
	
	Function ReadMemory:BlueBinary(mem:Byte Ptr, size:Int)
		Return Read(CreateRamStream(mem, size, 1, 0), "<ram stream>", size)
	End Function
	
	Function ReadObjectFile:BlueBinary(filename:String)
		Return Read(ReadFile(filename), filename, FileSize(filename))
	End Function
	
	Function Read:BlueBinary(file:TStream, filename:String, sz:Int)
		Local b:BlueBinary
		Try
			Local opc:BlueOpcode = New BlueOpcode
			Local corrupt:BlueAssemblyError = BlueAssemblyError.Make(filename, "not a valid Blue Moon binary file (possibly corrupt)")
			Local badInst:BlueError = corrupt.Make(filename, "invalid Blue Moon instruction (out of bounds error); could not load file")
			sz :- (8 + 8) ; If sz < 12 Or (sz Mod 4 <> 0) Then Throw corrupt
			
			Local magic:Long = file.ReadLong() ; If magic <> MAGICNUMBER Then Throw corrupt
			Local major:Int = file.ReadInt()
			If major <> BlueMoonVersion.MAJORVERSION Then Throw corrupt.Make(filename, "incompatible major version of interpreter for bytecode")
			Local minor:Int = file.ReadInt()
			If minor > BlueMoonVersion.MINORVERSION Then Throw corrupt.Make(filename, "incompatible version of interpreter for bytecode")
			
			Local fc:Int = file.ReadInt(), kc:Int = file.ReadInt(), koffs:Int = file.ReadInt(), headerSz:Int = 3 + fc
			sz :/ 4 ; If sz < headerSz + fc * 7 Or sz < koffs + kc * 3 Then Throw corrupt
			
			Local bufSz:Int = headerSz + (koffs - headerSz) * 2 + (sz - koffs), buf:Int[bufSz]
			buf[0] = fc ; buf[1] = kc ; buf[2] = koffs
			
			For Local fi:Int = 0 Until fc
				Local foff:Int = file.ReadInt() ; If sz < foff Then Throw corrupt
				buf[3 + fi] = headerSz + (foff - headerSz) * 2
			Next
			
			Local pos:Int = headerSz, sizeExtension:Int = 0
			For Local fi:Int = 0 Until fc
			'binSz :+ (7 ..	'id, instruction count, k count, param count, var count, upvar count, frame size
			 '      + 2 * f.upvars ..	'upvar levels and stack positions (interleaved)
			  '     + f.kc ..	'constant index table
			   '    + f.inst.Count())	'instructions
				Local id:Int = file.ReadInt(), icount:Int = file.ReadInt(), kcount:Int = file.ReadInt(), pcount:Int = file.ReadInt()
				Local vars:Int = file.ReadInt(), upvars:Int = file.ReadInt(), framesz:Int = file.ReadInt()
				If pos <> buf[3 + fi] Or id <> fi Or (icount|kcount|pcount|vars|upvars|framesz) < 0 Then Throw corrupt
				buf[pos] = id ; buf[pos + 1] = icount ; buf[pos + 2] = kcount ; buf[pos + 3] = pcount
				buf[pos + 4] = vars ; buf[pos + 5] = upvars ; buf[pos + 6] = framesz ; pos :+ 7
				If (pos - sizeExtension) + 2 * upvars + kcount + icount > sz Then Throw corrupt
				sizeExtension :+ (7 + 2 * upvars + kcount + icount) * 2
				
				For Local up:Int = 0 Until upvars
					buf[pos] = file.ReadInt() ; buf[pos + 1] = file.ReadInt() ; pos :+ 2
				Next
				For Local ki:Int = 0 Until kcount
					buf[pos] = file.ReadInt()
					If buf[pos] >= kc Then Throw corrupt
					pos :+ 1
				Next
				
				For Local i:Int = 0 Until icount
					Local in:Int = file.ReadInt(), op:Int, ra:Int, rb:Int, c:Int
					BlueOpcode.Read(in, op, ra, rb, c)
					
					Select op		'verify operation as not going out of bounds!
						Case opc.RETVA	'ignore
						Case opc.JMP, opc.JNOT, opc.JIF
							If i + c >= icount Or i + c < 0 Then Throw badinst
						Case opc.CALL, opc.TCALL
							If Max(ra, rb + c) >= framesz Or Min(ra, Min(rb, c)) < 0 Then Throw badInst
						Case opc.RET, opc.POSTCALL
							If ra + c >= framesz Or ra < 0 Or c < 0 Then Throw badInst
						Case opc.GETUPV
							If ra >= framesz Or ra < 0 Or rb >= upvars Or rb < 0 Then Throw badInst
						Case opc.SETUPV
							If rb >= framesz Or rb < 0 Or ra >= upvars Or ra < 0 Then Throw badInst
						Default
							If ra >= framesz Or rb >= framesz Or ra < 0 Or rb < 0 Then Throw badInst
							If (op >= opc.ADD And op <= opc.BSHR) Or (op >= opc.EQ And op <= opc.LEQ)
								If c >= framesz Or c < 0 Then Throw badInst
							Else
								Select op
									Case opc.GETTAB, opc.SETTAB, opc.LOADK
										If c >= kcount Or c < 0 Then Throw badInst
									Case opc.VARARG', opc.VAINIT, opc.CALLINIT
										
									Case opc.CLOSURE
										If c >= fc  Or c < 0 Then Throw badInst
								End Select
							EndIf
					End Select
					
					buf[pos] = op | (ra Shl 8) | (rb Shl 16) ; buf[pos + 1] = c
					pos :+ 2
				Next
				
				pos :+ (7 + 2 * upvars + kcount)	'unused space because of buffer doubling
			Next
			
			sizeExtension :/ 2
			If pos - sizeExtension <> koffs Then Throw corrupt
			buf[2] = pos	'update koffs
			For Local ki:Int = 0 To kc	'[sic] - we need to copy the end marker too
				Local koff:Int = file.ReadInt() ; If sz < koff Or koff < koffs + kc + 1 Then Throw corrupt
				buf[pos] = koff + sizeExtension ; pos :+ 1
			Next
			For Local ki:Int = 0 Until kc
				Local d:Double = file.ReadDouble(), dp:Int Ptr = Int Ptr(Varptr(d))
				buf[pos] = dp[0] ; buf[pos + 1] = dp[1] ; pos :+ 2
				If pos <> buf[koffs + sizeExtension + ki + 1]	'string
					For Local c:Int = 0 Until Ceil(d / 2)
						buf[pos + c] = file.ReadInt()
					Next
					pos :+ Ceil(d / 2)
				EndIf
			Next
			
			b = New Self ; b.buf = buf
		Catch o:Object
			CloseStream(file) ; Throw o
		End Try
		CloseStream(file)
		Return b
	End Function
	
	Method Write(file:TStream)
		Try
			file.WriteLong MAGICNUMBER
			file.WriteInt BlueMoonVersion.MAJORVERSION ; file.WriteInt BlueMoonVersion.MINORVERSION
			For Local i:Int = 0 Until out.Length
				file.WriteInt out[i]
			Next
		Catch o:Object
			CloseStream(file) ; Throw o
		End Try
		CloseStream(file)
	End Method
End Type

Type BlueAssembly
	Const MAXVARS:Int = 255, SI_LIMIT:Int = 131071, VSI_LIMIT:Int = 511
	Field fd:FunDef, id:Int
	Field vsp:Int
	Field ktbl:TList, kc:Int, kidx:Int[]
	Field inst:TList
	Field lbl:LabelDef[], gto:GotoDef[]
	Field params:Int, vars:Int, upvars:Int, framesz:Int, upvarTier:Int[], upvarStkPos:Int[]
	
	Function Make:BlueAssembly(fd:FunDef)
		Local f:BlueAssembly = New BlueAssembly
		f.fd = fd ; f.id = fd.id
		f.params = fd.pcount ; f.vars = fd.vmax ; f.upvars = fd.clos.Length
		f.vsp = f.vars '+ f.upvars
		f.upvarTier = New Int[f.upvars] ; f.upvarStkPos = New Int[f.upvars]
		For Local v:Int = 0 Until f.upvars
			Local el:Int = 1, env:FunDef = fd.env
			If env
				f.upvarStkPos[v] = -1
				For Local l:Int = 0 Until env.vars.Length
					If env.vars[l] = fd.clos[v] Then f.upvarStkPos[v] = l ; Exit
				Next
				If f.upvarStkPos[v] = -1
					For Local c:Int = 0 Until env.clos.Length
						If env.clos[c] = fd.clos[v]
							f.upvarTier[v] = 1 ; f.upvarStkPos[v] = c ; Exit
						EndIf
					Next
					Assert f.upvarStkPos[v] <> -1, "compile error: unable to resolve closure (critical compiler error)"
				EndIf
			Else
				f.upvarTier[v] = -1 ; f.upvarStkPos[v] = 0
			EndIf
		Next
		f.ktbl = CreateList() ; f.kc = 0
		Return f
	End Function
	
	Method Push:Int(n:Int = 1)
		vsp :+ n ; Return vsp - 1
	End Method
	Method Pop:Int(n:Int = 1)
		vsp :- n ; Return vsp
	End Method
	Method Peek:Int()
		Return vsp - 1
	End Method
	
	Method GetConst:Int(t:String, val:String)
		Return BlueBinary.AddKTblEntry(t + ":" + val, ktbl, kc)
	End Method
	
	Method ToString:String()
		Local s:String = "F: " + id + "~n"
		s :+ "  Constants:~n"
		For Local k:String = EachIn ktbl
			s :+ "    " + k + "~n"
		Next
		s :+ "  Upvars:~n"
		For Local v:Int = 0 Until upvars
			s :+ "    " + upvarTier[v] + "/" + upvarStkPos[v] + "~n"
		Next
		s :+ "  Instructions:~n"
		For Local i:BlueInstr = EachIn inst
			s :+ "    " + i.ToString() + "~n"
		Next
		Return s
	End Method
	
	Method Finish(_inst:TList, opc:BlueOpcode)
		inst = _inst
		lbl = LabelDef[](fd._lt.ToArray())
		gto = GotoDef[](fd._gt.ToArray())
		
		' allow closures over parameters
		For Local p:Int = params - 1 To 0 Step -1
			If fd.vars[p].isClosure Then inst.AddFirst(BlueInstr.Make(opc.NEWUPV, fd.vars[p].stkpos, 0))
		Next
		
		' add final return so that jumps don't run off the end
		inst.AddLast BlueInstr.Make(opc.RET, 0, 0, 0)
		' remove redundant MOV instructions
	'	Self.ReduceMovs(opc)
		' resolve targeted jumps and remove dummy target instructions
		Self.ResolveJumps(opc)
		' resolve break jumps and remove dummy target instructions
		Self.ResolveBreaks(opc)
		
		framesz = vars'upvars + vars
		For Local i:BlueInstr = EachIn inst
			Select i.op
				Case opc.JMP, opc.VARARG, opc.VAINIT, opc.CALLINIT, opc.RETVA	'ignore
				Case opc.CALL, opc.TCALL
					framesz = Max(framesz, Max(i.ra, i.rb + i.c) + 1)
				Case opc.RET, opc.POSTCALL
					framesz = Max(framesz, i.ra + i.c + 1)
				Default
					framesz = Max(framesz, i.ra + 1)
			End Select
		Next
		
		kidx = New Int[kc]
		fd = Null
	End Method
	
	Method ResolveJumps(opc:BlueOpcode)
		Local il:TLink = inst.FirstLink(), i:Int = 0
		While il
			Local in:BlueInstr = BlueInstr(il.Value())
			If in.op = opc.MOV And in.ra = 0 And in.rb = 0
				Local lab:LabelDef = lbl[in.c]
				lab.insl = il ; lab.insc = i
			ElseIf Not (in.op = opc.MOV And in.ra = in.rb)
				i :+ 1
			EndIf
			il = il.NextLink()
		Wend
		i = 0 ; il = inst.FirstLink()
		While il
			Local in:BlueInstr = BlueInstr(il.Value())
			If (in.op >= opc.JMP And in.op <= opc.JNOT) And in.c >= 0
				Local jmp:GotoDef = gto[in.c]
				in.c = jmp.tgt.insc - i ; jmp.tgt.insl.Remove()
			EndIf
			If Not (in.op = opc.MOV And in.ra = in.rb) Then i :+ 1
			il = il.NextLink()
		Wend
	End Method
	
	Method ResolveBreaks(opc:BlueOpcode)
		Local il:TLink = inst.FirstLink()
		While il
			Local in:BlueInstr = BlueInstr(il.Value())
			If in.op = opc.JMP And in.ra = -1
				Local tgl:TLink = il.NextLink(), cnt:Int = 1
				While True
					Local tgt:BlueInstr = BlueInstr(tgl.Value())
					If tgt.op = opc.MOV And tgt.c = -1
						in.ra = 0 ; in.rb = 0 ; in.c = cnt ; Exit
					EndIf
					tgl = tgl.NextLink() ; cnt :+ 1
				Wend
			EndIf
			il = il.NextLink()
		Wend
		il = inst.FirstLink()	'remove break point markers
		While il
			Local in:BlueInstr = BlueInstr(il.Value())
			If in.op = opc.MOV And in.c = -1 Then il.Remove()
			il = il.NextLink()
		Wend
	End Method
	
	Method ReduceMovs(opc:BlueOpcode)
		Function replaceMov1:TLink(ml:TLink Var, in:BlueInstr, ra:Int, rb:Int, c:Int)	'update a later instruction and then go back to look for more MOVs
			Local ret:TLink = ml.PrevLink()	'won't be null
			ml.Remove() ; ml = Null
			in.ra = ra ; in.rb = rb ; in.c = c
			Return ret
		End Function
		
		'first MOVs from variables to temps
		Local ml:TLink, mov:BlueInstr, il:TLink = inst.FirstLink(), vmax:Int = fd.vmax, tmpStk:Int = vmax' + fd.clos.Length
		While il
			Local in:BlueInstr = BlueInstr(il.Value())
			If in.op = opc.MOV	'if it's a move to temp space and nothing is pending
				If in.ra = in.rb	'jump target; do not cross
					ml = Null
				ElseIf ml = Null And in.ra >= tmpStk And in.rb < vmax
					ml = il ; mov = BlueInstr(ml.Value())
				ElseIf ml <> Null And in.rb = mov.ra And in.ra <> mov.rb
					il = replaceMov1(ml, in, in.ra, mov.rb, in.c)
				EndIf
			ElseIf ml <> Null
				If in.op >= opc.JMP	'any control-flow related op cancels
					If (in.op = opc.JIF Or in.op = opc.JNOT) And mov.ra = in.ra Then il = replaceMov1(ml, in, mov.rb, in.rb, in.c) Else il = ml
					ml = Null
				ElseIf in.op = opc.GETTAB Or in.op = opc.GETTABSI Or (in.op >= opc.UNM And in.op <= opc.UNP)
					If mov.ra = in.rb Then il = replaceMov1(ml, in, in.ra, mov.rb, in.c)
				ElseIf in.op = opc.SETTAB Or in.op = opc.SETTABSI
					If mov.ra = in.ra Then il = replaceMov1(ml, in, mov.rb, in.rb, in.c)
				ElseIf in.op = opc.GETTABI Or (in.op >= opc.ADD And in.op < opc.UNM) Or (in.op >= opc.EQ And in.op <= opc.LEQ)
					If mov.ra = in.rb
						il = replaceMov1(ml, in, in.ra, mov.rb, in.c)
					ElseIf mov.ra = in.c
						il = replaceMov1(ml, in, in.ra, in.rb, mov.rb)
					EndIf
				ElseIf in.op = opc.SETTABI
					If mov.ra = in.ra
						il = replaceMov1(ml, in, mov.rb, in.rb, in.c)
					ElseIf mov.ra = in.c
						il = replaceMov1(ml, in, in.ra, in.rb, mov.rb)
					EndIf
				EndIf
			EndIf
			If il = Null Then il = inst.FirstLink()
			il = il.NextLink()
		Wend
		ml = Null ; il = inst.LastLink()	'then MOVs from temps to variables
		While il
			Local in:BlueInstr = BlueInstr(il.Value())
			If in.op = opc.MOV
				If in.ra = in.rb
					ml = Null
				ElseIf ml = Null And in.ra < vmax And in.rb >= tmpStk
					ml = il ; mov = BlueInstr(ml.Value())
				EndIf
			ElseIf ml <> Null
				If in.op >= opc.JMP
					ml = Null
				ElseIf in.op = opc.GETTAB Or in.op = opc.GETTABI Or in.op = opc.GETTABSI Or in.op = opc.CLOSURE Or in.op = opc.NEWTAB Or ..
						(in.op >= opc.ADD And in.op <= opc.LEQ) Or (in.op >= opc.LOADK And in.op <= opc.LOADNIL)
					If mov.rb = in.ra
						il = ml.NextLink()	'won't be null
						ml.Remove() ; ml = Null ; in.ra = mov.ra
					EndIf
				EndIf
			EndIf
			il = il.PrevLink()
		Wend
	End Method
End Type

