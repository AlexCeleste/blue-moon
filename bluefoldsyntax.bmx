
' Blue Moon
' FOLD delegates to normalize and check syntax, fold constant expressions

' this file is a Private Include

'SuperStrict

'Import "blueerror.bmx"
'Import "TFold.bmx"
'Import "blueparser.bmx"


Type NormalizeFold
	Field tbl:TMap
	Function Make:NormalizeFold(tbl:NodeTable)
		Local f:NormalizeFold = New NormalizeFold ; f.tbl = tbl.leaves ; Return f
	End Function
	
	Method LeftAssociative:Node(n:Rose) { SumExpr MulExpr AndExpr RelExpr Expr }	'restructure left-associative operations from a list to a tree
		Local l:Node = n.Get("L"), r:Rose = Rose(n.Get("R"))
		For Local c:Int = 0 Until r.arg.Length
			Local rarg:Rose = Rose(r.arg[c])
			l = Rose.Make(rarg.arg[0].key, [l, rarg.arg[1]], ["L", "R"])
		Next
		Return l
	End Method
	Method RightAssociative:Node(n:Rose) { CatExpr PowExpr }	'properly structure right-associative operations
		Local l:Node = n.Get("L"), r:Rose = Rose(n.Get("R"))
		Return Rose.Make(r.arg[0].key, [l, r.arg[1]], ["L", "R"])
	End Method
	Method UnaryExpr:Node(n:Rose)	'remove duplicate nots and negations, completely remove unary posate
		Local ops:Rose = Rose(n.Get("op")), ret:Node = n.Get("arg")
		Local op:String[ops.arg.Length], prevK:String = "", count:Int
		For Local c:Int = 0 Until ops.arg.Length
			Local key:String = ops.arg[c].key ; If key = "minus" Then key = "negate"
			If prevK = key And (key = "negate" Or key = "knot")
				count :- 1 ; If count Then prevK = op[count - 1] Else prevK = ""
			ElseIf key <> "plus"
				op[count] = key ; count :+ 1 ; prevK = key
			EndIf
		Next
		For Local c:Int = count - 1 To 0 Step -1
			ret = Rose.Make(op[c], [ret], Null)
		Next
		Return ret
	End Method
	Method TableCons:Node(n:Rose)	'remove redundant levels from table constructor expressions
		n = Rose(n.Get("fields"))
		If n
			Local rst:Int = 0 ; If n.arg.Length > 1 Then rst = Rose(n.arg[1]).arg.Length
			Local arg:Node[] = New Node[1 + rst]
			For Local c:Int = 1 Until arg.Length
				arg[c] = Rose(Rose(n.arg[1]).arg[c - 1]).arg[1]
			Next
			arg[0] = n.arg[0]
			Local icount:Int = 1
			For Local c:Int = 0 Until arg.Length
				Local ac:Rose = Rose(arg[c])
				
				If arg[c].key = "FieldKey"	'these are actually useful unconverted (optimization)
					arg[c] = Rose.Make("FieldKey", [Node(Leaf.Make("fname", Leaf(ac.arg[0]).val)), ac.arg[1]], ac.id)
					
				ElseIf c = arg.Length - 1 And arg[c].key = "dotdotdot"	'vararg initialization
					arg[c] = Leaf.Make("va_init", "...")
					
				ElseIf c = arg.Length - 1 And (arg[c].key = "fcall" Or arg[c].key = "mcall")	'var-ret initialization
					arg[c] = Rose.Make("va_" + ac.key, ac.arg, Null)
					
				ElseIf arg[c].key <> "FieldIndex"	'convert basic expressions to indexed expressions
					Local idx:Node = Leaf.Make("dconst", icount) ; icount :+ 1
					arg[c] = Rose.Make("FieldIndex", [idx, arg[c]], ["key", "val"])
				EndIf
			Next
			arg :+ [Leaf.Make("icount", icount)]
			Return Rose.Make("TableCons", arg)
		Else
			Return Rose.Make("TableCons", Null)
		EndIf
	End Method
	
	Method LocalVar:Node(n:Rose) { Assgt }	'balance sides of assignment or initialization
		Local vals:Rose = Rose(n.Get("vals")), vars:Rose = Rose(n.arg[0])
		If vals = Null Then vals = Rose.Make("ExpList", [Leaf.Make("nil", "nil")])
		
		If vals.arg.Length < vars.arg.Length
			Local lastkey:String = vals.arg[vals.arg.Length - 1].key, extend:Int = Max(1, vars.arg.Length - (vals.arg.Length - 1))
			
			If lastkey <> "fcall" And lastkey <> "mcall" And lastkey <> "dotdotdot"	'polyvalue assignment doesn't extend
				Local nv:Node[] = vals.arg[..vars.arg.Length], nil:Node = Leaf.Make("nil", "nil")
				For Local v:Int = vals.arg.Length Until nv.Length
					nv[v] = nil
				Next
				vals = Rose.Make("ExpList", nv)
			ElseIf lastkey = "dotdotdot"	'mark required vararg values
				Local valarg:Node[] = vals.arg[..]
				valarg[valarg.Length - 1] = Leaf.Make("dotdotdot", extend)
				vals = Rose.Make(vals.key, valarg)
			Else	'adjust return count
				Local call:Rose = Rose(vals.arg[vals.arg.Length - 1]), carg:Node[] = call.arg[..]
				carg[2] = Leaf.Make("rcount", extend)
				call = Rose.Make(call.key, carg)
				Local valarg:Node[] = vals.arg[..] ; valarg[valarg.Length - 1] = call
				vals = Rose.Make(vals.key, valarg)
			EndIf
			
		ElseIf vars.arg.Length < vals.arg.Length
			Local l:Int = vars.arg.Length ; vars = Rose.Make(vars.key, vars.arg[..vals.arg.Length])
			For Local v:Int = l Until vals.arg.Length		'too many vals: discard into a fake var
				vars.arg[v] = Leaf.Make("discard", "discard")
			Next
		EndIf
		
		Return Rose.Make(n.key, [vars, vals], ["vars", "vals"])
	End Method
	
	Method Block:Node(n:Rose)	'remove empty statements
		Local old:Rose = Rose(n.arg[0])
		If old
			Local st:Node[] = New Node[old.arg.Length], count:Int = 0
			For Local s:Int = 0 Until old.arg.Length
				Local k:String = old.arg[s].key ; If k <> "semicolon" And k <> "#Nil"
					st[count] = old.arg[s] ; count :+ 1
				EndIf
			Next
			If count
				Return Rose.Make("Block", st[..count])
			Else
				Return Rose.Make("Block", Null)
			EndIf
		ElseIf NilNode(n.arg[0])
			Return Rose.Make("Block", Null)
		EndIf
		Return n
	End Method
	Method DoBlk:Node(n:Rose)	'extract block; reduce indirection
		Return n.arg[0]
	End Method
	Method IfS:Node(n:Rose)	'rewrite if/elseif chains into nested regular if/then/else
		Local elif:Rose = Rose(n.Get("elif")), _else:Node = n.Get("else")
		If elif
			For Local e:Int = elif.arg.Length - 1 To 0 Step -1
				If _else
					_else = Rose.Make("IfS", Rose(elif.arg[e]).arg + [_else], ["test", "then", "else"])
				Else
					_else = Rose.Make("IfS", Rose(elif.arg[e]).arg, ["test", "then"])
				EndIf
			Next
			Return Rose.Make("IfS", n.arg[..2] + [_else], ["test", "then", "else"])
	'	Else
	'		Return Rose.Make("IfS", n.arg + [Rose.Make("Block", Null)], ["test", "then", "else"])
		EndIf
		Return n
	End Method
	Method ForN:Node(n:Rose)	'convert for loops to equivalent scoped-whiles
		Global rewrite:String = ..
		"~n	local var, limit, step = tonumber(e1), tonumber(e2), tonumber(e3)" +..	'building this with Make calls would be insane, and increase error surface
		"~n	if not (var and limit and step) then error() end" +..
		"~n	var = var - step" +..
		"~n	while true do" +..
		"~n		var = var + step" +..
		"~n		if (step >= 0 and var > limit) or (step < 0 and var < limit) then" +..
		"~n			break" +..
	 	"~n		end" +..
	 	"~n		local _var = var" +..
		"~n		--block" +..
	 	"~n	end~n"
		
		Global rwast:TParseNode ; If rwast = Null Then rwast = BlueParseString(rewrite)
		Local rwtbl:NodeTable = Node.FromParse(rwast), rw:Node = rwtbl.n
		rw = Node.Fold(rw, NormalizeFold.Make(rwtbl))
		
		Local v:Node = n.arg[0], f:Node = n.arg[1], t:Node = n.arg[2], s:Node = n.Get("step"), b:Node = n.Get("block")
		If s = Null Then s = Leaf.Make("dconst", "1")	'ensure step is present
		rw = Node.Fold(rw, FindReplaceFold.Make( ..
			["var", "limit", "step", "e1", "e2", "e3", "_var"], ..	'insert actual names and values into template
			[Node(Leaf.Make("name", "::var")), Node(Leaf.Make("name", "::limit")), Node(Leaf.Make("name", "::step")), f, t, s, v]))
		
		Rose(Rose(Rose(rw).arg[3]).arg[1]).arg :+ [b]
		Return rw
	End Method
	Method ForIn:Node(n:Rose)
		Global rewrite:String = ..
		"~n	local f, s, var = explist" +..	'doing it this way reduces error surface by reusing more of the same code machinery
		"~n	while true do" +..
		"~n		local var_1, var_2, var_n = f(s, var)" +..
		"~n		if var_1 == nil then break end" +..
		"~n		var = var_1" +..
		"~n		--block" +..
		"~n	end~n"	'the outer `do` is removed here because it's added implicitly by the parser anyway
		
		Global rwast:TParseNode ; If rwast = Null Then rwast = BlueParseString(rewrite)
		Local rwtbl:NodeTable = Node.FromParse(rwast), rw:Node = rwtbl.n
		rw = Node.Fold(rw, NormalizeFold.Make(rwtbl))
		
		Local va:Node = n.arg[0], v1:Node = Rose(va).arg[0], b:Node = n.arg[2]
		rw = Node.Fold(rw, FindReplaceFold.Make( ..
			["f", "s", "var", "var_1"], ..	'insert actual names and values into template
			[Node(Leaf.Make("name", "::f")), Node(Leaf.Make("name", "::s")), Node(Leaf.Make("name", "::var")), v1]))
		
		Rose(Rose(rw).arg[0]).arg[1] = n.arg[1]	'replace the expression list at declaration of ::f/::s/::var
		Local wstmts:Node[] = Rose(Rose(Rose(rw).arg[1]).arg[1]).arg
		wstmts :+ [b]	'add the loop body to the list of statements in the new while loop
		Rose(wstmts[0]).arg[0] = n.arg[0]	'loop variables
		
		Return rw
	End Method
	
	Method FunDecl:Node(n:Rose)	'replace function declarations with simple assignment
		Local name:Node = n.Get("name"), body:Rose = Rose(n.Get("body"))
		If name.key = "MethodName"
			Local params:Rose = Rose(body.arg[0])
			params = Rose.Make(params.key, [Leaf.Make("name", "self")] + params.arg)
			body = Rose.Make(body.key, [Node(params), body.arg[1]], body.id)
			name = Rose.Make("getfield", Rose(name).arg)
		EndIf
		Return Rose.Make("Assgt", [Rose.Make("VarList", [name]), Rose.Make("ExpList", [body])], ["vars", "vals"])
	End Method
	Method FuncName:Node(n:Rose)	'convert function name expressions to var+fields (but leave trailing method name to FunDecl)
		Local r:Node = n.Get("n"), path:Rose = Rose(n.Get("path")), meth:Rose = Rose(n.Get("method"))
		If path
			For Local pe:Rose = EachIn path.arg
				Local acc:Node = Leaf.Make("fname", Leaf(pe.arg[1]).val)
				r = Rose.Make("getfield", [r, acc])
			Next
		EndIf
		If meth
			Local mname:Node = Leaf.Make("fname", Leaf(meth.arg[1]).val)
			r = Rose.Make("MethodName", [r, mname])	'package for FunDecl to rebrand as a get
		EndIf
		Return r
	End Method
	Method LocalFun:Node(n:Rose)	'replace local function declarations with local assignment
		Local nam:Node = n.Get("name"), dec:Node = Rose.Make("LocalVar", [..
			Rose.Make("NameList", [nam]), Rose.Make("ExpList", [Leaf.Make("nil", "nil")])], ["names", "vals"])	'nil-init'd
		Local asg:Node = Rose.Make("Assgt", [Rose.Make("VarList", [nam]), Rose.Make("ExpList", [n.Get("body")])], ["vars", "vals"])
		Return Rose.Make("#EXPAND", [dec, asg])
	End Method
	Method FuncBody:Node(n:Rose)	'reduce indirection on parameter list and straighten out varargs
		Local param:Node = Rose(n.arg[0]).arg[0], plist:Rose = Rose(param)
		If NilNode(param)
			param = Rose.Make("NameList", Null)
		ElseIf param.key = "dotdotdot"
			param = Rose.Make("NameList", [param])
		ElseIf plist And plist.arg.Length = 2 And plist.arg[1].key = "dotdotdot"
			param = Rose.Make("NameList", Rose(plist.arg[0]).arg + [plist.arg[1]])
		EndIf
		Return Rose.Make(n.key, [param, n.arg[1]], n.id)
	End Method
	
	Method GotoS:Node(n:Rose) { Label }
		Local l:Leaf = Leaf(n.arg[0]), t:Object = tbl.ValueForKey(l) ; tbl.Remove(l)	'modifying an error-able leaf means updating its error entry
		l = Leaf.Make(n.key, l.val) ; tbl.Insert(l, t)
		Return l
	End Method
	Method ReturnS:Node(n:Rose)
		Local vals:Node[] = Null
		If Rose(n.arg[0]) Then vals = Rose(n.arg[0]).arg
		Return Rose.Make(n.key, vals)
	End Method
	
	Method PostfixExp:Node(n:Rose)	'rewrite element access/function call chains into nested single ops
		Local f:Node = n.arg[0], pl:Node[] = Rose(n.arg[1]).arg
		For Local e:Int = 0 Until pl.Length
			Select pl[e].key
				Case "sconst", "TableCons"
					f = Rose.Make("fcall", [f, Node(Rose.Make("FArgs", [pl[e]])), Node(Leaf.Make("rcount", "1"))])
				Case "FArgs"
					f = Rose.Make("fcall", [f, pl[e], Node(Leaf.Make("rcount", "1"))])	'add number of returned values
				Case "MArgs"
					Leaf(Rose(pl[e]).arg[0]).key = "fname"
					f = Rose.Make("mcall", [f, pl[e], Node(Leaf.Make("rcount", "1"))])
				Case "ElemGet"
					f = Rose.Make("index", [f, Rose(pl[e]).arg[0]])
				Case "FieldGet"
					Local acc:Node = Rose(pl[e]).arg[0] ;  acc = Leaf.Make("fname", Leaf(acc).val)
					f = Rose.Make("getfield", [f, acc])
			End Select
		Next
		Return f
	End Method
	Method FArgs:Node(n:Rose)	'clear empty arg lists and reduce indirection
		Local al:Rose = Rose(n.arg[0])
		If al Then Return Rose.Make("FArgs", al.arg) Else Return Rose.Make("FArgs", Null)
	End Method
	
	Method dotdotdot:Node(n:Leaf)
		Return Leaf.Make("dotdotdot", "1")
	End Method
End Type

Type FindReplaceFold
	Field names:String[], vals:Node[]
	
	Function Make:FindReplaceFold(names:String[], vals:Node[])
		Local f:FindReplaceFold = New Self
		f.names = names ; f.vals = vals ; Return f
	End Function
	
	Method name:Node(n:Leaf)	'replace occurrences of names in the name array with the matching vals value
		For Local i:Int = 0 Until names.Length
			If names[i] = n.val Then Return vals[i]
		Next
		Return n
	End Method
End Type

Type ConstantFold	'fold constant math and logic operations
	Method hconst:Node(n:Leaf) { hfconst }	'convert hex constants to uniform representation (unnecessary? BlitzMax natively supports 0x1a23p-4 format)
		Return Leaf.Make("dconst", String(Double(n.val)))
	End Method
	
	Method BinMathOp:Node(n:Rose) { plus minus mul div kmod pow }
		Const DK:String = "dconst"
		If n.arg[0].key = DK And n.arg[1].key = DK
			Local l:Double = Double(Leaf(n.arg[0]).val), r:Double = Double(Leaf(n.arg[1]).val)
			Select n.key
				Case "plus"  ; l :+ r
				Case "minus" ; l :- r
				Case "mul"   ; l :* r
				Case "div"   ; l :/ r
				Case "kmod"  ; l = l Mod r
				Case "pow"   ; l = l ^ r
			End Select
			Return Leaf.Make(DK, l)
		Else
			Return n
		EndIf
	End Method
	Method UnaryOp:Node(n:Rose) { negate klen knot }
		Local a:Leaf = Leaf(n.arg[0]) ; If a = Null Then Return n
		Select n.key
			Case "negate"
				If a.key = "dconst" Or a.key = "sconst"		'BlitzMax natively supports Lua number formats, it's just the C one
					Return Leaf.Make("dconst", String(-Double(a.val)))
				EndIf
			Case "not"
				Select a.key
					Case "nil", "bconst"
						If a.val = "true" Then Return Leaf.Make("bconst", "false") Else Return Leaf.Make("bconst", "true")
					Case "dconst", "sconst"
						Return Leaf.Make("bconst", "false")
				End Select
		End Select
		Return n
	End Method
	Method AndExpr:Node(n:Rose) { Expr }	'optimize short-circuit operators with constant first operand
		Local l:Leaf = Leaf(n.arg[0])
		If l
			If l.key = "sconst" Or l.key = "dconst" Or (l.key = "bconst" And l.val = "true")
				If n.key = "kor" Then Return l Else Return n.arg[1]
			ElseIf l.key = "nil" Or (l.key = "bconst" And l.val = "false")
				If n.key = "kor" Then Return n.arg[1] Else Return l
			EndIf
		EndIf
		Return n
	End Method
	Method eq:Node(n:Rose) { neq }
		Local l:Leaf = Leaf(n.arg[0]), r:Leaf = Leaf(n.arg[1])
		If l <> Null And r <> Null And l.key = r.key
			Global rv:String[] = ["false", "true"]
			Select l.key
				Case "bconst", "sconst" ; Return Leaf.Make("bconst", rv[(n.key = "eq") * (l.val = r.val)])
				Case "dconst" ; Return Leaf.Make("bconst", rv[(n.key = "eq") * (Double(l.val) = Double(r.val))])	'it's fine when they're constants
			End Select
		EndIf
		Return n
	End Method
	Method lt:Node(n:Rose) { gt leq geq }
		Local l:Leaf = Leaf(n.arg[0]), r:Leaf = Leaf(n.arg[1])
		If l <> Null And r <> Null And l.key = "dconst" And r.key = "dconst"	'string comparisons are affected by runtime locale
			Global rv:String[] = ["false", "true"] ; Local ret:Int
			Select n.key
				Case "gt" ; ret = Double(l.val) > Double(r.val)
				Case "lt" ; ret = Double(l.val) < Double(r.val)
				Case "geq" ; ret = Double(l.val) >= Double(r.val)
				Case "neq" ; ret = Double(l.val) <= Double(r.val)
			End Select
			Return Leaf.Make("bconst", rv[ret])
		EndIf
		Return n
	End Method
	
	Method IfS:Node(n:Rose)	'unpack constant if structures
		Local test:Leaf = Leaf(n.arg[0])
		If test
			Select test.key
				Case "bconst", "dconst", "sconst", "nil"
					If (test.key = "bconst" And test.val = "False") Or test.key = "nil"
						If n.Get("else") Then Return n.Get("else") Else Return Null
					Else
						Return n.Get("then")
					EndIf
			End Select
		EndIf
		Return n
	End Method
	Method WhileS:Node(n:Rose)	'remove while(false)
		Local test:Leaf = Leaf(n.arg[0])
		If test And ((test.key = "bconst" And test.val = "false") Or (test.key = "nil")) Then Return Null
		Return n
	End Method
	Method RepeatS:Node(n:Rose)	'replace repeat...until(false) with a while
		Local t:Leaf = Leaf(n.Get("test"))
		If t
			If t.key = "nil" Or (t.key = "bconst" And t.val = "false")
				Return Rose.Make("WhileS", [Node(Leaf.Make("bconst", "true")), n.Get("block")], ["test", "block"])
		'	ElseIf t.key = "dconst" Or t.key = "sconst" Or (t.key = "bconst" And t.val = "true")
		'		Return n.Get("block")
			EndIf
		EndIf
		Return n
	End Method
End Type

Type SyntaxErrFold	'report any remaining syntax errors
	Field tbl:TMap, breaks:TList
	Function Make:SyntaxErrFold(tbl:NodeTable)
		Local f:SyntaxErrFold = New SyntaxErrFold
		f.tbl = tbl.leaves ; f.breaks = CreateList()
		Return f
	End Function
	
	Method Block:Node(n:Rose)	'scan for non-statements
		For Local s:Int = 0 Until n.arg.Length
			Select n.arg[s].key
				Case "fcall", "mcall"
					n.arg[s].key = "p" + n.arg[s].key
				Case "Assgt", "break", "GotoS", "Label", "Block", "WhileS", "RepeatS", "IfS", "LocalVar", "ReturnS"', "ForN", "ForIn"
				Default
					Throw BlueCompileError.Make(TToken(tbl.ValueForKey(n.arg[s])), "invalid statement in statement block")
			End Select
		Next
		Return n
	End Method
	
	Method break:Node(n:Leaf)	'add breaks to a stack...
		breaks.AddFirst(n) ; Return n
	End Method
	Method AllLoops:Node(n:Rose) { WhileS RepeatS } 'ForN ForIn }	'...then remove them
		breaks.Clear() ; Return n
	End Method
	Method FuncBody:Node(n:Rose)	'...and error on any that weren't removed
		Self.BreakCheck() ; Return n
	End Method
	
	Method BreakCheck()	'need to call this externally for toplevel too
		If breaks.Count() Then Throw BlueCompileError.Make(TToken(tbl.ValueForKey(breaks.First())), "'break' statement without containing loop")
	End Method
End Type

