
' Blue Moon
' FOLD delegates to analyze semantics (build function and variable tables, check for semantic errors)

' this file is a Private Include

'SuperStrict

'Import "blueerror.bmx"
'Import "TFold.bmx"


' this module folds through the AST to extract function bodies and resolve references to variables and labels
' local variables are replaced by references to their location in the function's local table (not their stack
' position, which they hold internally - two variables on the same stack pos may not be identical yet)
' closure variables are replaced by references to their location in the function's closure table


Type SemanticFold
	Field funs:TList, _ft:TList, fcount:Int, tbl:TMap
	
	Function Make:SemanticFold(root:Node, tbl:NodeTable)
		Local f:SemanticFold = New SemanticFold
		f.funs = CreateList() ; f._ft = CreateList() ; f.tbl = tbl.leaves
		FunDef.Toplevel(root, f)
		Return f
	End Function
	Function Close(semf:SemanticFold, r:Node)
		Local toplevel:FunDef = FunDef(semf.funs.First())
		toplevel.Close(Rose.Make("FuncBody", [Node(Rose.Make("NameList", Null, Null)), r], ["param", "body"]), semf)
	End Function
	
	Method FuncBody:Node(n:Rose)	'Add function definition to global list
		Local f:FunDef = FunDef.Make(n, Self), params:Rose = Rose(n.arg[0]), pa:Node[] = params.arg[..]
		If pa Then f.hasVarargs = (pa[pa.Length - 1].key = "dotdotdot")
		f.vars = New VarDef[pa.Length]
		For Local p:Int = 0 Until pa.Length	'add parameters
			Local par:Leaf = Leaf(pa[p])
			pa[p] = Leaf.Make("param", par.val)
			If par.key = "name"
				f.vars[p] = f.AddVar(par, Self)
			Else	'drop the ...
				pa = pa[..pa.Length - 1] ; f.vars = f.vars[..pa.Length] ; Exit
			EndIf
		Next
		f.pcount = pa.Length
		Return Rose.Make(n.key, [Node(Rose.Make(params.key, pa)), n.arg[1]], n.id)
	End Method
	Method FuncBody2:Node(n:Rose)	'extract body and pop env
		Local f:FunDef = FunDef(_ft.RemoveFirst()), env:FunDef = FunDef(_ft.First())
		f.Close(n, Self)
		For Local c:VarDef = EachIn f.clos
			If Not env._vt.Contains(c) Then env.AddClos(c)	'make sure all closures are one-level at most
		Next
		Return RefLeaf.Make("funref", NumToStr(f.id))
	End Method
	
	Method Block:Node(n:Rose)	'enter a scope level
		Local f:FunDef = FunDef(_ft.First()) ; f._ev.AddFirst n
		Return n
	End Method
	Method Block2:Node(n:Rose)	'exit a scope level
		Local f:FunDef = FunDef(_ft.First()), n2:Node = Node(f._ev.RemoveFirst()) ; f.CloseBlock n2, Self	'note that n itself cannot be tested for equality
		Return n
	End Method
	
	Method RepeatS:Node(n:Rose)	'repeat scope extends to the end of the test, need to put that inside it (never need to remove it, either)
		Local blk:Rose = Rose.Make(n.arg[0].key, Rose(n.arg[0]).arg + [n.arg[1]])
		Return Rose.Make(n.key, [blk])
	End Method
	
	'for loops are now replaced during normalization
'	Method ForN:Node(n:Rose) { ForIn }	'for loops create scope blocks with variables
'		Local blk:Node = n.Get("block")
'		Local f:FunDef = FunDef(_ft.First()) ; f._ev.AddFirst blk
'		If n.key = "ForN"
'			Local arg:Node[] = n.arg[..], v:VarDef = f.AddVar(Leaf(arg[0]), Self)
'			arg[0] = RefLeaf.Make("l_varref", NumToStr(v.uid))
'			n = Rose.Make(n.key, arg, n.id)
'		Else
'			Local arg:Node[] = Rose(n.arg[0]).arg[..]
'			For Local i:Int = 0 Until arg.Length
'				Local v:VarDef = f.AddVar(Leaf(arg[i]), Self)
'				arg[i] = RefLeaf.Make("l_varref", NumToStr(v.uid))
'			Next
'			n = Rose.Make(n.key, [Node(Rose.Make(n.arg[0].key, arg, Null))] + n.arg[1..], n.id)
'		EndIf
'		Return n
'	End Method
'	Method ForN2:Node(n:Rose) { ForIn2 }	'pop the scope
'		Local f:FunDef = FunDef(_ft.First()) ; f._ev.RemoveFirst()
'		Return n
'	End Method
	
	Method LocalVar:Node(n:Rose)	'prevent declared names from getting in the way
		Local vars:Node[] = Rose(n.arg[0]).arg[..]
		For Local i:Int = 0 Until vars.Length
			Local t:Object = tbl.ValueForKey(vars[i]) ; tbl.Remove(vars[i])	'need to replace nodes in the err table if they're still potential error points
			vars[i] = Leaf.Make("lname", Leaf(vars[i]).val)
			tbl.Insert(vars[i], t)
		Next
		Return Rose.Make(n.key, [Node(Rose.Make("VarList", vars)), n.arg[1]], n.id)
	End Method
	Method LocalVar2:Node(n:Rose)	'define vars on the way back up
		Local vars0:Node[] = Rose(n.arg[0]).arg, vars:Node[vars0.Length], vr:Node = Rose.Make("VarList", vars)
		For Local i:Int = 0 Until vars.Length
			Local v:VarDef = FunDef(_ft.First()).AddVar(Leaf(vars0[i]), Self)	'notice we only add the name on the way back up
			vars[vars.Length - (i + 1)] = RefLeaf.Make("l_varref", NumToStr(v.uid))	'proceed backward to reflect stack discipline
		Next
		Return Rose.Make(n.key, [n.arg[1], vr], ["vals", "vars"])	'swap vars and vals so vals are evaluated first
	End Method
	
	Method Assgt2:Node(n:Rose)	'convert lvalue keys to indicate different action
		Local vars0:Node[] = Rose(n.arg[0]).arg, vars:Node[vars0.Length], vr:Node = Rose.Make("VarList", vars)
		For Local v:Int = 0 Until vars0.Length
			vars0[v].key = "l_" + vars0[v].key ; vars[vars.Length - (v + 1)] = vars0[v]
		Next
		Return Rose.Make(n.key, [n.arg[1], vr], ["vals", "vars"])	'swap, as above
	End Method
	
	Method name:Node(n:Leaf)	'var ref
		Function getvar:VarDef(_ft:TList, n:String)
			For Local f:FunDef = EachIn _ft
				For Local v:VarDef = EachIn f._vt
					If v.name = n
						If f <> _ft.First() Then v.isClosure = True
						Return v
					EndIf
				Next
				For Local v:VarDef = EachIn f._ct
					If v.name = n Then Return v
				Next
			Next
		End Function
		Local v:VarDef = getvar(_ft, n.val), f:FunDef = FunDef(_ft.First()) ; If v
			If v.isClosure
				Local id:Int = f.AddClos(v)	'add to the list of closure vars to retain
				Return RefLeaf.Make("closref", NumToStr(id))
			Else
				Return RefLeaf.Make("varref", NumToStr(v.uid))
			EndIf
		Else	'free, i.e. _ENV.n
			v = getvar(_ft, "_ENV")
			Local id:Int = f.AddClos(v), env:Node = RefLeaf.Make("closref", NumToStr(id)), name:Node = Leaf.Make("fname", n.val)
			Return Rose.Make("getfield", [env, name], Null)
		EndIf
	End Method
	Method fcall:Node(n:Rose) { mcall pfcall pmcall tfcall tmcall va_fcall va_mcall }	'count arguments to function calls
		Local ac:Int
		Select n.key
			Case "fcall", "pfcall", "tfcall", "va_fcall"
				ac = Rose(n.arg[1]).arg.Length
			Default	'*mcall
				ac = Rose(Rose(n.arg[1]).arg[1]).arg.Length
		End Select
		Return Rose.Make(n.key, n.arg + [Leaf.Make("acount", ac)], Null)
	End Method
	
	Method Label:Node(n:Leaf) { GotoS }	'jump or dest
		Local f:FunDef = FunDef(_ft.First()), val:Int
		If n.key = "Label" Then val = f.AddLabel(n, Self) Else val = f.AddGoto(n)
		Return RefLeaf.Make(n.key, NumToStr(val))
	End Method
	
	Method ReturnS2:Node(n:Rose)	'detect and mark tail calls (on the way up to give the call a chance to process)
		If n.arg.Length = 1
			Local k:String = n.arg[0].key, c:Rose = Rose(n.arg[0])
			If k = "fcall" Or k = "mcall"
				Return Rose.Make("t" + k, c.arg, c.id)
			ElseIf k = "dotdotdot"
				Return Leaf.Make("return_va", "...")
			EndIf
		EndIf
		Return n
	End Method
End Type

Type FunDef
	Field n:Node, id:Int, env:FunDef
	Field vars:VarDef[], clos:VarDef[], pcount:Int, stksz:Int, vmax:Int, vcount:Int, hasVarargs:Int
	Field _ev:TList, _vt:TList, _ct:TList, _lt:TList, _gt:TList
	
	Function Make:FunDef(blk:Node, in:SemanticFold)
		Local f:FunDef = New FunDef
		f.env = FunDef(in._ft.First())
		f._vt = CreateList() ; f._ev = CreateList() ; f._ct = CreateList() ; f._lt = CreateList() ; f._gt = CreateList()
		in.funs.AddLast f ; in._ft.AddFirst f
		f.id = in.fcount ; in.fcount :+ 1
		Return f
	End Function
	Function Toplevel:FunDef(root:Node, in:SemanticFold)	'create a chunk toplevel as a function whose sole parameter is _ENV
		Local f:FunDef = FunDef.Make(root, in), env:VarDef = VarDef.Make("_ENV", Null, 0, 0, -1)
		env.isClosure = True ; f._ct.AddLast env ; Return f
	End Function
	Method AddVar:VarDef(n:Leaf, in:SemanticFold)
		Local blk:Node = Node(_ev.First()), v:VarDef
		For v = EachIn _vt
			If v.blk <> blk Then Exit	'the error here doesn't actually apply in standard Lua
		'	If v.name = n.val Then Throw CompileError.Make(TToken(in.tbl.ValueForKey(n)), "duplicate local variable '" + n.val + "'")
		Next
		v = VarDef.Make(n.val, blk, stksz, vcount, id) ; vcount :+ 1
		_vt.AddFirst v ; stksz :+ 1 ; Return v
	End Method
	Method AddClos:Int(v:VarDef)
		Local c:Int
		For Local e:VarDef = EachIn _ct
			If v = e Then Return c Else c :+ 1
		Next
		_ct.AddLast v ; Return c
	End Method
	Method AddLabel:Int(n:Leaf, in:SemanticFold)
		Local l:LabelDef = New LabelDef ; l.name = n.val ; l.depth = _ev.Count() ; l.varsvisible = stksz
		For Local ln:LabelDef = EachIn _lt
			If ln.depth = l.depth And ln.name = l.name Then Throw BlueCompileError.Make(TToken(in.tbl.ValueForKey(n)), "duplicate label '::" + n.val + "::'")
		Next
		_lt.AddLast l ; Return _lt.Count() - 1
	End Method
	Method AddGoto:Int(n:Leaf)
		Local g:GotoDef = New GotoDef ; g.name = n.val ; g.depth = _ev.Count() ; g.varsvisible = stksz ; g._n = n
		_gt.AddLast g ; Return _gt.Count() - 1
	End Method
	Method ResolveGotos(in:SemanticFold)
		Function cmp:Int(l:Object, r:Object)
			If LabelDef(l).depth > LabelDef(r).depth Return 1 ElseIf LabelDef(l).depth < LabelDef(r).depth Return -1 Else Return 0
		End Function
		_lt.Sort True, cmp
		For Local g:GotoDef = EachIn _gt
			Local l:LabelDef = Null
			For Local _l:LabelDef = EachIn _lt
				If g.depth < _l.depth Then Exit
				If g.name = _l.name Then l = _l ; Exit
			Next
			If l
				g.tgt = l ; g._n = Null
			Else
				Throw BlueCompileError.Make(TToken(in.tbl.ValueForKey(g._n)), "label '::" + g._n.val + "::' not visible in scope")
			EndIf
		Next
	End Method
	Method CloseBlock(n:Node, in:SemanticFold)
		vmax = Max(vmax, stksz)
		Local c:Int = 0, oldLen:Int = vars.Length
		For Local v:VarDef = EachIn _vt
			If v.blk <> n Then Exit
			c :+ 1 ; v.blk = Null	'noretain
		Next
		vars = New VarDef[c] + vars
		For Local i:Int = 0 Until c
			vars[i] = VarDef(_vt.RemoveFirst())	'pop contained vars
		Next
		stksz :- c
	End Method
	Method Close(_n:Node, in:SemanticFold)
		n = _n ; clos = VarDef[](_ct.ToArray()) ; ResolveGotos in
		Local v2:VarDef[] = New VarDef[vars.Length]		'vars aren't in the right order due to params, sort by uid
		For Local vi:Int = 0 Until v2.Length
			Local v:VarDef = vars[vi] ; v2[v.uid] = v
		Next
		vars = v2
		_vt = Null ; _ev = Null ; _ct = Null '; _lt = Null ; _gt = Null
	End Method
End Type

Type VarDef
	Field name:String, blk:Node, isClosure:Int, stkpos:Int, uid:Int, fid:Int
	Function Make:VarDef(n:String, b:Node, stkpos:Int, uid:Int, fid:Int)
		Local v:VarDef = New VarDef ; v.name = n ; v.stkpos = stkpos ; v.uid = uid ; v.fid = fid ; v.blk = b ; Return v
	End Function
End Type

Type LabelDef
	Field name:String, depth:Int, varsvisible:Int, insl:TLink, insc:Int = -1
End Type

Type GotoDef
	Field name:String, depth:Int, varsvisible:Int, _n:Leaf, tgt:LabelDef', op:Object
End Type


Type RefLeaf Extends Leaf
	Function Make:Leaf(key:String, val:String)
		Local l:Leaf = New RefLeaf
		l.key = key ; l.val = val
		Return l
	End Function
	Method ToString:String()
		Return "{ '#" + StrToNum(val) + "' : " + key + " }"
	End Method
End Type
Function StrToNum:Int(s:String)
	Return s[0] | (s[1] Shl 8) | (s[2] Shl 16) | (s[3] Shl 24)
End Function
Function NumToStr:String(n:Int)
	Return Chr(n & $FF) + Chr((n & $FF00) Shr 8) + Chr((n & $FF0000) Shr 16) + Chr((n & $FF000000) Shr 24)
End Function


