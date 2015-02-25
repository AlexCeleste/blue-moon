
' Blue Moon
' Lua-table implementation


SuperStrict

Import "blueallocator.bmx"


Private
Extern
	Function frexp:Double(d:Double, Exp:Int Ptr) = "frexp"
End Extern
Public

Type BlueTable Final
	Function Get:Long(tbl:Byte Ptr, key:Long)
		Local tag:Int = Int Ptr(Varptr(key))[1], idx:Int
		
		If tag = BlueTypeTag.NANBOX | BlueTypeTag.STR	'string - common case
			idx = Int Ptr(Int(key))[1]
		ElseIf tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX	'number
			Local d:Double ; Long Ptr(Varptr(d))[0] = key ; idx = Abs Int(d)
			If d = idx	'int key
				Local arr:Byte Ptr = Byte Ptr Ptr(tbl)[3]
				If arr And idx < Int Ptr(arr)[-1] Then Return Long Ptr(arr)[idx]	'if this is nil, it won't be in the hash part anyway
			Else
				d = frexp(d, Varptr(idx)) * ($7fffffff - 1024)	'INT_MAX - DBL_MAX_EXP
				idx = Abs(idx) + Int(d)
			EndIf
		ElseIf tag = BlueTypeTag.NANBOX | BlueTypeTag.NIL	'nil -> nil
			Return key
		Else	'pointer (needs improvement)
			idx = Int(key) Shr 3
		EndIf
		
		Local hashpart:Byte Ptr = Byte Ptr Ptr(tbl)[2]
		If hashpart
			Local hsize:Int = 1 Shl Int Ptr(hashpart)[-1]
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
	
	Function Set(mem:BlueVMMemory, tbl:Byte Ptr, key:Long, val:Long)
		Local tag:Int = Int Ptr(Varptr(key))[1], idx:Int, slot:Long Ptr = Null
		
		If tag = BlueTypeTag.NANBOX | BlueTypeTag.NIL Then Return	'nil is not a valid key
		
		If tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX	'if key is an int and less than arraylength
			Local d:Double ; Long Ptr(Varptr(d))[0] = key ; idx = Abs Int(d)
			If d = idx
				Local arraypart:Byte Ptr = Byte Ptr Ptr(tbl)[3]
				If arraypart <> Null And idx < Int Ptr(arraypart)[-1] Then slot = Long Ptr(arraypart) + idx
			EndIf
		EndIf
		If slot = Null	'elsewise if hashcount < hashsize
			Local hashpart:Byte Ptr = Byte Ptr Ptr(tbl)[2]
			If hashpart <> Null
				Local hsize:Int = 1 Shl Int Ptr(hashpart)[-1]
				If Int Ptr(hashpart)[-2] < hsize	'there's a slot free somewhere
					
					If tag = BlueTypeTag.NANBOX | BlueTypeTag.STR	'string - common case
						idx = Int Ptr(Int(key))[1]
					ElseIf tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX	'non-integer
						Local d:Double ; Long Ptr(Varptr(d))[0] = key
						d = frexp(d, Varptr(idx)) * ($7fffffff - 1024)	'INT_MAX - DBL_MAX_EXP
						idx = Abs(idx) + Int(d)
					Else	'pointer
						idx = Int(key) Shr 3
					EndIf
					
					idx = idx & (hsize - 1)	'fastmod
					For Local i:Int = idx Until hsize
						If Long Ptr(hashpart)[2 * i] = key Then slot = Long Ptr(hashpart) + (2 * i + 1)
					Next
					For Local i:Int = 0 Until idx
						If Long Ptr(hashpart)[2 * i] = key Then slot = Long Ptr(hashpart) + (2 * i + 1)
					Next
					
				EndIf
			EndIf
		EndIf
		
		If slot = Null Then slot = Resize(mem, tbl, key)
		mem.Write(slot, val)
	End Function
	
	Function Resize:Long Ptr(mem:BlueVMMemory, tbl:Byte Ptr, key:Long)
	End Function
End Type

