
' Blue Moon
' Lua-table implementation


SuperStrict

Import "blueallocator.bmx"


Type BlueTable Final
	Function Set(tbl:Byte Ptr, key:Long, val:Long)
		
	End Function
	Function Get:Long(tbl:Byte Ptr, key:Long)
		Local tag:Int = Int Ptr(Varptr(key))[1], idx:Int
		
		If tag = BlueTypeTag.NANBOX | BlueTypeTag.STR	'string - common case
			idx = Int Ptr(Int(key))[1]
		ElseIf tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX	'number
			Local d:Double ; Long Ptr(Varptr(d))[0] = key ; idx = Int(d)
			If d = idx	'int key
				Local arr:Byte Ptr = Byte Ptr Ptr(tbl)[3]
				If idx < Int Ptr(arr)[0]
					'if it's not nil
					Return Long Ptr(arr)[idx + 1]
				EndIf
			Else
				Extern
					Function frexp:Double(d:Double, Exp:Int Ptr) = "frexp"
				End Extern
				d = frexp(d, Varptr(idx)) * ($7fffffff - 1024)	'INT_MAX - DBL_MAX_EXP
				idx = Abs(idx) + Int(d)
			EndIf
		ElseIf tag = BlueTypeTag.NANBOX | BlueTypeTag.NIL	'nil -> nil
			Return key
		Else	'pointer (needs improvement)
			idx = Int(key) Shr 3
		EndIf
		
		
	End Function
	Function Resize()
	End Function
End Type

