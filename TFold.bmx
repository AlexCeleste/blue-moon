
' Generic tree rewriting FOLD engine
' Define operations on nodes as methods with names or metadata matching the node key
' Fold delegates do not have to conform to any interface at all (reflection will find a method or it won't)

' this file is mainly meant to be used as a Private Include

'SuperStrict

'Import "TMeta.bmx"	'Get this here: http://www.blitzbasic.com/codearcs/codearcs.php?code=3090

'Import Brl.Map
'Import Brl.Reflection


Type Node Abstract
	Field key:String
	Const UP:Int = 1, DOWN:Int = 2, BOTH:Int = UP | DOWN
	
	Function Fold:Node(n:Node, f:Object, dir:Int = Node.UP)
		Function fold0:Node(n:Node, f:Object, cache:TMap, dir:Int)
			Local m:TMethod = TMethod(cache.ValueForKey(n.key)) ; If Not m Then m = TMethod(cache.ValueForKey("any"))
			If (dir & Node.DOWN) And m Then n = Node(m.Invoke(f, [n]))	'fold down (root -> leaves, not default)
			
			Local r:Rose = Rose(n)
			If r
				Local arg:Node[] = New Node[r.arg.Length], count:Int = 0
				For Local i:Int = 0 Until arg.Length
					Local folded:Node = fold0(r.arg[i], f, cache, dir)
					If folded
						If folded.key = "#EXPAND"
							arg = arg[..count] + Rose(folded).arg + (New Node[r.arg.Length - i])
							count :+ Rose(folded).arg.Length
						Else
							arg[count] = folded ; count :+ 1
						EndIf
					EndIf
				Next
				n = Rose.Make(r.key, arg[..count], r.id)
			EndIf
			
			If (dir = Node.UP) And m
				n = Node(m.Invoke(f, [n]))	'fold up (leaves -> root, default)
			ElseIf dir = (Node.UP | Node.DOWN)
				m = TMethod(cache.ValueForKey(n.key + "2"))
				If m Then n = Node(m.Invoke(f, [n]))	'fold both (not default): invoke Method2 on the return
			EndIf
			Return n
		End Function
		
		Local cache:TMap = BuildCache(f)
		Return fold0(n, f, cache, dir)
	End Function
	
	Function FromParse:NodeTable(pt:TParseNode)
		Function from:Node(pt:TParseNode, leaves:TMap)
			If pt.term
				Local l:Leaf = Leaf.Make(pt.term.tType, pt.term.value)
				leaves.Insert(l, pt.term) ; Return l
			ElseIf pt.elem
				Local arg:node[] = New Node[pt.elem.Length], id:String[] = New String[arg.Length]
				For Local e:Int = 0 Until arg.Length
					arg[e] = from(pt.elem[e], leaves) ; id[e] = pt.name[e]
				Next
				Return Rose.Make(pt.rule, arg, id)
			Else
				Return NilNode.Get()
			EndIf
		End Function
		
		Local map:TMap = CreateMap()
		Return NodeTable.Make(from(pt, map), map)
	End Function
	
	Function BuildCache:TMap(del:Object)
		Local cache:TMap = TMap(_delegateMapCache.ValueForKey(TTypeId.ForObject(del)))
		If cache <> Null Then Return cache
		
		cache = CreateMap()
		For Local m:TMethod = EachIn TTypeId.ForObject(del).Methods()
			cache.Insert(m.Name(), m)
			Local alts:String = m.MetaData(), k:Int = alts.Find("=")
			While k >= 0
				cache.Insert(alts[..k], m)
				alts = alts[k + 3..]
				k = alts.Find("=")
			Wend
		Next
		
		_delegateMapCache.Insert(TTypeId.ForObject(del), cache)
		Return cache
	End Function
	
	Global _delegateMapCache:TMap = CreateMap()	'save reparsing the metadata every time we want to read a file
End Type


Type NilNode Extends Node
	Function Get:Node()
		Global n:NilNode
		If Not n Then n = New NilNode ; n.key = "#Nil"
		Return n
	End Function
	Method ToString:String()
		Return "[NIL]"
	End Method
End Type

Type Leaf Extends Node
	Field val:String
	Function Make:Leaf(key:String, val:String)
		Local l:Leaf = New Leaf
		l.key = key ; l.val = val
		Return l
	End Function
	Method ToString:String()
		Return "{ '" + val + "' : " + key + " }"
	End Method
End Type

Type Rose Extends Node
	Field arg:Node[], id:String[]
	Function Make:Rose(key:String, arg:Node[], id:String[] = Null)
		Local r:Rose = New Rose
		r.key = key ; r.arg = arg ; r.id = id
		Return r
	End Function
	Method Get:Node(n:String)
		If id = Null Then Return arg[Int(n[1..])]
		For Local e:Int = 0 Until arg.Length
			If id[e] = n Then Return arg[e]
		Next
		Return Null
	End Method
	Method Set:Node(n:String, val:Node)
		If id = Null Then arg[Int(n[1..])] = val
		For Local e:Int = 0 Until arg.Length
			If id[e] = n Then arg[e] = val ; Exit
		Next
		Return Self
	End Method
	Method ToString:String()
		Function show:String(r:Rose, ind:Int)
			Local s:String = r.key + "~n", pad:String = LSet("", ind)
			For Local i:Int = 0 Until r.arg.Length
				Local nam:String ; If r.id Then nam = r.id[i] Else nam = "@" + i
				s :+ pad + nam + ": "
				If Rose(r.arg[i])
					s :+ show(Rose(r.arg[i]), ind + 2)
				Else
					s :+ r.arg[i].ToString() + "~n"
				EndIf
			Next
			If r.arg.Length = 0 Then s :+ pad + "[NIL]~n"
			Return s
		End Function
		Return show(Self, 2)
	End Method
End Type


Type NodeTable Final
	Field n:Node
	Field leaves:TMap
	Function Make:NodeTable(n:Node, m:TMap)
		Local t:NodeTable = New NodeTable ; t.n = n ; t.leaves = m ; Return t
	End Function
End Type

