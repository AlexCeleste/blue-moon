
' Blue Moon
' Lua-table implementation


SuperStrict

Import "blueallocator.bmx"


Type BlueTable Final
	Function Get:Long(tbl:Byte Ptr, key:Long)
		Local tag:Int = Int Ptr(Varptr(key))[1], idx:Int
		
		If tag = BlueTypeTag.NANBOX | BlueTypeTag.STR	'string - common case
			idx = Int Ptr(Int(key))[1]
		ElseIf tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX	'number
			Local d:Double ; Long Ptr(Varptr(d))[0] = key ; idx = Int(d)
			If d = idx	'int key
				Local arr:Byte Ptr = Byte Ptr Ptr(tbl)[3]
				If arr And idx < Int Ptr(arr)[1] Then Return Long Ptr(arr)[idx + 1]	'if this is nil, it won't be in the hash part anyway
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
		
		Local hashpart:Byte Ptr = Byte Ptr Ptr(tbl)[2] + 8
		If hashpart
			hsize:Int = 1 Shl Int Ptr(hashpart)[-1]
			idx = idx & (hsize - 1)	'apparently this is faster than Mod
			For Local i:Int = idx Until hsize	'naive linear probe
				If Long Ptr(hashpart)[2 * i] = key Then Return Long Ptr(hashpart)[2 * i + 1]
			Next
			For Local i:Int = 0 Until idx	'yep
				If Long Ptr(hashpart)[2 * i] = key Then Return Long Ptr(hashpart)[2 * i + 1]
			Next
		EndIf
		Local ret:Long ; Int Ptr(Varptr(ret))[1] = BlueTypeTag.NANBOX | BlueTypeTag.NIL	'why can't i shift longs?
		Return ret
	End Function
	
	Function Set(tbl:Byte Ptr, key:Long, val:Long)
		Local tag:Int = Int Ptr(Varptr(key))[1], idx:Int
		
		
	End Function
	
	Function Resize()
	End Function
End Type

