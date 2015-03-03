
' This FOLD delegate is intended for debugging others

'enumerate keys (compiler debug tool: make sure all are handled and none are wrong)
'this prints all keys not handled by the given fold delegate (given nothing, it just prints all keys)
Type Debug_KeyListFold
	Field cache:TMap, handled:TMap
	Function Make:Debug_KeyListFold(over:Object = Null)
		Local d:Debug_KeyListFold = New Self
		d.cache = CreateMap() ; If over Then d.handled = Node.BuildCache(over)
		Return d
	End Function
	
	Function Run(funs:FunDef[], over:Object)
		Local dbg:Debug_KeyListFold = Self.Make(over)
		For Local f:FunDef = EachIn funs
			Node.Fold(f.n, dbg)
		Next
		If Not dbg.cache.IsEmpty() Then Print "Debug nodes:"
		For Local s:String = EachIn dbg.cache.Keys()
			Print "  " + s
		Next
	End Function
	
	Method any:Node(n:Node)
		If handled = Null Or handled.ValueForKey(n.key) = Null Then cache.Insert(n.key, "@")
		Return n
	End Method
End Type

