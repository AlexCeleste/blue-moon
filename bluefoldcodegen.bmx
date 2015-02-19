
' Blue Moon
' FOLD delegate to generate assembly instructions

' this file is a Private Include

'SuperStrict


Type CompileFold
	Field opc:BlueOpcode = New BlueOpcode, bi:BlueInstr = New BlueInstr, f:BlueAssembly, _gq:TList = CreateList()
	
	Function Make:CompileFold(f:BlueAssembly)
		Local c:CompileFold = New Self ; c.f = f ; Return c
	End Function
	
	Method l_varref:Node(n:Leaf)
		Local v:VarDef = f.fd.vars[StrToNum(n.val)], op:Int
		If v.isClosure Then op = opc.SETLC Else op = opc.MOV
		Return BytecodeNode.MakeL(n.key, bi.Make(op, v.stkpos, f.Pop()), n)
	End Method
	Method l_closref:Node(n:Leaf)
		Local ref:Int = StrToNum(n.val)
		Return BytecodeNode.MakeL(n.key, bi.Make(opc.SETUPV, ref, f.Pop()))
	End Method
	Method l_getfield:Node(n:Rose)
		Local r:BytecodeNode = BytecodeNode.Join(n.key, [n.arg[0]])
		Local k:Int = f.GetConst("1", Leaf(n.arg[1]).val), tab:Int = f.Pop()
		r.inst.AddLast bi.Make(opc.SETTAB, tab, f.Pop(), k)
		Return r
	End Method
	Method l_index:Node(n:Rose)
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg)
		Local ind:Int = f.Pop(), tab:Int = f.Pop(), val:Int = f.Pop()
		Local pv:BlueInstr = BlueInstr(r.inst.Last())
		If pv.op = opc.LOADSI
			pv.op = opc.SETTABSI ; pv.ra = tab ; pv.rb = val
		Else
			r.inst.AddLast bi.Make(opc.SETTABI, tab, val, ind)
		EndIf
		Return r
	End Method
	Method l_discard:Node(n:Node)
		f.Pop() ; Return BytecodeNode.Nil()
	End Method
	
	Method LocalVar:Node(n:Rose)
		Local vars:Node[] = Rose(BytecodeNode(n.arg[1]).old).arg
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg)
		For Local vi:Int = vars.Length - 1 To 0 Step -1
			Local v:VarDef = f.fd.vars[StrToNum(Leaf(BytecodeNode(vars[vi]).old).val)]
			If v.isClosure Then r.inst.AddFirst bi.Make(opc.NEWUPV, v.stkpos, 0)
		Next
		Return r
	End Method
	
	Method ReturnS:Node(n:Rose)
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg)
		r.inst.AddLast(bi.MakeK(opc.RET, f.Peek(), n.arg.Length))
		f.Pop(n.arg.Length) ; Return r
	End Method
	Method return_va:Node(n:Node)
		Return BytecodeNode.MakeL(n.key, bi.Make(opc.RETVA, 0, 0))
	End Method
	
	Method break:Node(n:Node)
		Return BytecodeNode.MakeL(n.key, bi.Make(opc.JMP, -1, -1, -1))
	End Method
	Method RepeatS:Node(n:Rose)
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg), _lt:TList = f.fd._lt, _gt:TList = f.fd._gt
		Local loopStart:LabelDef = New LabelDef, lsID:Int = _lt.Count(), loop:GotoDef = New GotoDef, lID:Int = _gt.Count()
		loop.tgt = loopStart ; _lt.AddLast loopStart ; _gt.AddLast loop
		Local lastop:BlueInstr = BlueInstr(r.inst.Last()), reg:Int = f.Pop()
		If lastop.op = opc.LNOT
			lastop.op = opc.JIF ; lastop.ra = lastop.rb ; lastop.rb = 0 ; lastop.c = lID	'replace NOT with inverted jump
		Else
			r.inst.AddLast bi.MakeK(opc.JNOT, reg, lID)
		EndIf
		r.inst.AddFirst bi.Make(opc.MOV, 0, 0, lsID)
		r.inst.AddLast bi.Make(opc.MOV, -1, -1, -1)
		Return r
	End Method
	Method IfS:Node(n:Rose) { WhileS }
		Local r:BytecodeNode = BytecodeNode.Join(n.key, [n.arg[0]]), lastop:BlueInstr = BlueInstr(r.inst.Last())
		Local before:LabelDef, befID:Int, loopRound:GotoDef, loopID:Int, _lt:TList = f.fd._lt, _gt:TList = f.fd._gt
		If n.key = "WhileS"
			before = New LabelDef ; befID = _lt.Count() ; _lt.AddLast before
			loopRound = New GotoDef ;loopRound.tgt = before ; loopID = _gt.Count() ; _gt.AddLast loopRound
			r.inst.AddFirst bi.Make(opc.MOV, 0, 0, befID)
		EndIf
		Local postThen:LabelDef = New LabelDef, skipThen:GotoDef = New GotoDef, ptID:Int = _lt.Count(), stID:Int = _gt.Count()
		_lt.AddLast postThen ; skipThen.tgt = postThen ; _gt.AddLast skipThen
		If lastop.op = opc.LNOT
			lastop.op = opc.JIF ; lastop.ra = lastop.rb ; lastop.rb = 0 ; lastop.c = stID	'replace NOT with inverted jump
		Else
			r.inst.AddLast bi.MakeK(opc.JNOT, f.Peek(), stID)
		EndIf
		f.Pop()
		For Local op:BlueInstr = EachIn BytecodeNode(n.arg[1]).inst
			r.inst.AddLast op
		Next
		Local postElse:LabelDef, skipElse:GotoDef
		If n.arg.Length > 2	'there's an else
			postElse = New LabelDef ; skipElse = New GotoDef ; skipElse.tgt = postElse
			Local peID:Int = _lt.Count(), seID:Int = _gt.Count()
			_lt.AddLast postElse ; _gt.AddLast skipElse
			r.inst.AddLast bi.MakeK(opc.JMP, 0, seID)
			r.inst.AddLast bi.Make(opc.MOV, 0, 0, ptID)
			For Local op:BlueInstr = EachIn BytecodeNode(n.arg[2]).inst
				r.inst.AddLast op
			Next
			r.inst.AddLast bi.Make(opc.MOV, 0, 0, peID)
		Else
			If n.key = "WhileS" Then r.inst.AddLast bi.MakeK(opc.JMP, 0, loopID) ; r.inst.AddLast bi.Make(opc.MOV, -1, -1, -1)
			r.inst.AddLast bi.Make(opc.MOV, 0, 0, ptID)
		EndIf
		Return r
	End Method
	Method kand:Node(n:Rose) { kor }
		Local r:BytecodeNode = BytecodeNode.Join(n.key, [n.arg[0]]), lastop:BlueInstr = BlueInstr(r.inst.Last())
		Local _lt:TList = f.fd._lt, _gt:TList = f.fd._gt
		Local postR:LabelDef = New LabelDef, skipR:GotoDef = New GotoDef, prID:Int = _lt.Count(), srID:Int = _gt.Count()
		_lt.AddLast postR ; skipR.tgt = postR ; _gt.AddLast skipR
		Local reg:Int = f.Pop()
		If lastop.op = opc.LNOT
			Local op:Int = opc.JIF ; If n.key = "kor" Then op = opc.JNOT
			lastop.op = op ; lastop.ra = lastop.rb ; lastop.rb = 0 ; lastop.c = srID	'replace NOT with inverted jump
		Else
			Local op:Int = opc.JNOT ; If n.key = "kor" Then op = opc.JIF
			r.inst.AddLast bi.MakeK(op, reg - 1, srID)
		EndIf
		For Local op:BlueInstr = EachIn BytecodeNode(n.arg[1]).inst
			r.inst.AddLast op
		Next
		r.inst.AddLast bi.Make(opc.MOV, reg - 1, reg)
		r.inst.AddLast bi.Make(opc.MOV, 0, 0, prID)
		Return r
	End Method
	
	Method fcall:Node(n:Rose) { pfcall tfcall va_fcall }	'"p" prefix = "procedure" i.e. no return value required
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg), argc:Int = Int(Leaf(n.arg[3]).val), retc:Int = Int(Leaf(n.arg[2]).val)
		Local func:Int = f.Pop(argc + 1), args:Int = func + 1, op:Int = opc.CALL
		If n.key = "tfcall" Then op = opc.TCALL
		r.inst.AddLast bi.Make(op, func, args, argc)
		If n.key = "fcall"
			r.inst.AddLast bi.MakeK(opc.POSTCALL, f.Peek() + 1, retc) ; f.Push(retc)
		ElseIf n.key = "va_fcall"
			r.inst.AddLast bi.Make(opc.CALLINIT, -1, 0)
		EndIf
		Return r
	End Method
	Method mcall:Node(n:Rose) { pmcall tmcall va_mcall }
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg), argc:Int = Int(Leaf(n.arg[3]).val), retc:Int = Int(Leaf(n.arg[2]).val)
		Local func:Int = f.Push(), argv:Int = f.Pop(argc + 2), op:Int = opc.CALL
		Local ks:String = Leaf(Rose(BytecodeNode(n.arg[1]).old).arg[0]).val		'yeesh
		r.inst.AddLast bi.Make(opc.GETTAB, func, argv, f.GetConst("1", ks))
		If n.key = "tmcall" Then op = opc.TCALL
		r.inst.AddLast bi.Make(op, func, argv, argc + 1)
		If n.key = "mcall"
			r.inst.AddLast bi.MakeK(opc.POSTCALL, argv, retc) ; f.Push(retc)
		ElseIf n.key = "va_fcall"
			r.inst.AddLast bi.Make(opc.CALLINIT, -1, 0)
		EndIf
		Return r
	End Method
	Method dotdotdot:Node(n:Leaf)
		Local retc:Int = Int(n.val), r:BytecodeNode
		r = BytecodeNode.MakeL(n.key, bi.MakeK(opc.VARARG, f.Peek() + 1, retc))
		f.Push(retc)
		Return r
	End Method
	
	Method BinaryOp:Node(n:Rose) { plus minus mul div kmod pow dotdot lt gt leq geq eq neq }
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg), op:Int
		Select n.key
			Case "plus"   ; op = opc.ADD
			Case "minus"  ; op = opc.SUB
			Case "mul"    ; op = opc.MUL
			Case "div"    ; op = opc.DIV
			Case "kmod"   ; op = opc.NMOD
			Case "pow"    ; op = opc.POW
			Case "dotdot" ; op = opc.CAT
			
			Case "lt"     ; op = opc.LT
			Case "gt"     ; op = opc.LEQ
			Case "leq"    ; op = opc.LEQ
			Case "geq"    ; op = opc.LT
			Case "eq"     ; op = opc.EQ
			Case "neq"    ; op = opc.EQ
		End Select
		Local lop:Int = f.Pop(), rop:Int = f.Pop()
		If n.key = "geq" Or n.key = "gt"
			r.inst.AddLast bi.Make(op, f.Push(), rop, lop)	'technically this violates IEEE-754, but it's how Lua defines it
		Else
			r.inst.AddLast bi.Make(op, f.Push(), lop, rop)
			If n.key = "neq" Then r.inst.AddLast bi.Make(opc.LNOT, f.Peek(), f.Peek())
		EndIf
		Return r
	End Method
	Method UnaryOp:Node(n:Rose) { negate klen knot }
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg), op:Int
		Select n.key
			Case "negate" ; op = opc.UNM
			Case "klen"   ; op = opc.ALEN
			Case "knot"   ; op = opc.LNOT
			Case "posate" ; op = opc.UNP
		End Select
		Local arg:Int = f.Pop()
		r.inst.AddLast bi.Make(op, f.Push(), arg)
		Return r
	End Method
	
	Method dconst:Node(n:Leaf) { sconst }
		Local ki:Int = Int(n.val), op:Int = opc.LOADK
		If n.key = "dconst" And Abs(ki) < BlueAssembly.SI_LIMIT And Double(n.val) = Double(ki)
			op = opc.LOADSI
		Else
			ki = f.GetConst(String(n.key = "sconst"), n.val)
		EndIf
		Return BytecodeNode.MakeL(n.key, bi.MakeK(op, f.Push(), ki))
	End Method
	Method bconst:Node(n:Leaf) { nil }
		Local ins:BlueInstr
		Select n.val
			Case "true"  ; ins = bi.MakeK(opc.LOADBOOL, f.Push(), 1)
			Case "false" ; ins = bi.MakeK(opc.LOADBOOL, f.Push(), 0)
			Case "nil"   ; ins = bi.MakeK(opc.LOADNIL, f.Push(), 0)
		End Select
		Return BytecodeNode.MakeL(n.key, ins)
	End Method
	
	Method funref:Node(n:Leaf) { varref closref }
		Local i:BlueInstr, ref:Int = StrToNum(n.val)
		Select n.key
			Case "funref" ; i = bi.MakeK(opc.CLOSURE, f.Push(), ref)
			Case "varref"
				Local v:VarDef = f.fd.vars[ref], op:Int = opc.MOV ; If v.isClosure Then op = opc.GETLC
				i = bi.Make(op, f.Push(), v.stkpos)
			Case "closref" ; i = bi.Make(opc.GETUPV, f.Push(), ref)' + f.fd.vmax)
		End Select
		Return BytecodeNode.MakeL(n.key, i)
	End Method
	Method getfield:Node(n:Rose) { index }
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg), i:BlueInstr
		If n.key = "getfield"
			Local k:Int = f.GetConst("1", Leaf(n.arg[1]).val), tab:Int = f.Pop()
			r.inst.AddLast bi.Make(opc.GETTAB, f.Push(), tab, k)
		Else
			Local idx:Int = f.Pop(), tab:Int = f.Pop()
			Local pv:BlueInstr = BlueInstr(r.inst.Last())
			If pv.op = opc.LOADSI And Abs(pv.c) < f.VSI_LIMIT
				pv.op = opc.GETTABSI ; pv.ra = f.Push() ; pv.rb = tab
			Else
				r.inst.AddLast bi.Make(opc.GETTABI, f.Push(), tab, idx)
			EndIf
		EndIf
		Return r
	End Method
	
	Method TableCons:Node(n:Rose)
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg), fld:BytecodeNode = Null
		Local rslot:Int = f.Push(), tslot:Int = f.vars '+ f.upvars
		For fld = EachIn n.arg	'work out where it's safe to put the table
			For Local in:BlueInstr = EachIn fld.inst
				If in.ra >= tslot Then tslot = in.ra + 1
			Next
		Next
		For fld = EachIn n.arg	'then patch the operations to point there
			For Local in:BlueInstr = EachIn fld.inst
				If in.ra = -1 And (in.op = opc.SETTAB Or in.op = opc.SETTABI ..
					Or in.op = opc.VAINIT Or in.op = opc.CALLINIT) Then in.ra = tslot
			Next
		Next
		If n.arg	'pass the ongoing index count to the variable initializer
			fld = BytecodeNode(n.arg[n.arg.Length - 2])
			If fld.key = "va_fcall" Or fld.key = "va_init"
				Local in:BlueInstr = BlueInstr(fld.inst.Last())
				in.c = Int(Leaf(n.arg[n.arg.Length - 1]).val)
			EndIf
		EndIf
		If tslot = -1 Then tslot = rslot Else tslot = f.Push(tslot - f.Peek())
		r.inst.AddFirst bi.Make(opc.NEWTAB, tslot, 0)
		If tslot <> rslot Then r.inst.AddLast bi.Make(opc.MOV, rslot, tslot)	'put the resulting table in the expected slot
		f.Pop(f.Peek() - rslot)	'...and correct the stack
		Return r
	End Method
	Method FieldIndex:Node(n:Rose) { FieldKey }
		Local r:BytecodeNode = BytecodeNode.Join(n.key, n.arg)
		Local val:Int = f.Pop(), idx:Int, op:Int
		If n.key = "FieldIndex"
			idx = f.Pop() ; op = opc.SETTABI
		Else
			idx = f.GetConst("1", Leaf(n.arg[0]).val) ; op = opc.SETTAB
		EndIf
		r.inst.AddLast bi.Make(op, -1, val, idx)
		Return r
	End Method
	Method va_init:Node(n:Node)
		Return BytecodeNode.MakeL(n.key, bi.Make(opc.VAINIT, -1, 0))
	End Method
	
	Method Label:Node(n:Leaf)	'insert dummy instructions to remove in the next pass
		Return BytecodeNode.MakeL(n.key, bi.Make(opc.MOV, 0, 0, StrToNum(n.val)))
	End Method
	Method GotoS:Node(n:Leaf)	'resolve these in the next pass after removing dummy targets
		Return BytecodeNode.MakeL(n.key, bi.MakeK(opc.JMP, 0, StrToNum(n.val)))
	End Method
	
	Method PASSTHROUGH:Node(n:Node) { FuncBody Block ExpList FArgs MArgs NameList VarList Assgt }
		If Rose(n)
			Return BytecodeNode.Join(n.key, Rose(n).arg, n)	'combine instruction sequences where they exist
		Else
			Return BytecodeNode.Nil(n.key, n)
		EndIf
	End Method
	Method IGNORE:Node(n:Node) { fname param rcount acount icount }
		Return n	'some things don't represent instructions at all and should be passed as data nodes
	End Method
	Method any:Node(n:Node)
		If Rose(n)
			Print "unhandled (passthrough): " + n.key
			Return BytecodeNode.Join(n.key, Rose(n).arg, n)
		Else
			Print "unhandled: " + n.key
			Return BytecodeNode.Nil(n.key, n)
		EndIf
	End Method
End Type

Type BytecodeNode Extends Node
	Field inst:TList, old:Node
	Function MakeL:BytecodeNode(key:String, ins:BlueInstr, old:Node = Null)
		Local n:BytecodeNode = New Self ; n.key = key ; n.inst = CreateList() ; n.old = old
		n.inst.AddLast(ins) ; Return n
	End Function
	Function MakeR:BytecodeNode(key:String, inst:BlueInstr[], old:Node = Null)
		Local n:BytecodeNode = New Self ; n.key = key ; n.inst = TList.FromArray(inst) ; n.old = old ; Return n
	End Function
	Function Join:BytecodeNode(key:String, els:Node[], old:Node = Null)
		Local n:BytecodeNode = New Self ; n.key = key ; n.inst = CreateList() ; n.old = old
		For Local e:BytecodeNode = EachIn els	'use of Eachin is important, as it autocasts
			For Local o:Object = EachIn e.inst
				n.inst.AddLast o
			Next
		Next
		Return n
	End Function
	Function Nil:BytecodeNode(key:String = "@", old:Node = Null)
		Return MakeR(key, Null, old)
	End Function
End Type

