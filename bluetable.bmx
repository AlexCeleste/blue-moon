
' Blue Moon
' Lua-table implementation
' uses simple linear probing for collision resolution and a shockingly naive reallocator


SuperStrict

Import "blueallocator.bmx"


Private
Extern
	Function frexp:Double(d:Double, Exp:Int Ptr) = "frexp"
End Extern
Public

Type BlueTable Final
	' retrieve the slot *for* a key's value, null if no available slot exists (to be used by non-raw index/set operations)
	Function GetSlot:Long Ptr(tbl:Byte Ptr, key:Long, keyslot:Long Ptr Ptr)	'keyslot will return the slot for the key if it's not an array-slot
		Local tag:Int = Int Ptr(Varptr(key))[1], idx:Int
		Const NILTAG:Int = BlueTypeTag.NANBOX | BlueTypeTag.NIL
		
		If tag = BlueTypeTag.NANBOX | BlueTypeTag.STR	'string - common case
			idx = Int Ptr(Int(key))[1]
		ElseIf tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX	'number
			Local d:Double ; Long Ptr(Varptr(d))[0] = key ; idx = Abs Int(d)
			If d = idx	'int key
				Local arr:Byte Ptr = Byte Ptr Ptr(tbl)[3]
				If arr And idx < Int Ptr(arr)[-1] Then Return Long Ptr(arr) + idx	'if this is nil, it won't be in the hash part anyway
			Else
				d = frexp(d, Varptr(idx)) * ($7fffffff - 1024)	'INT_MAX - DBL_MAX_EXP
				idx = Abs(idx) + Int(d)
			EndIf
		ElseIf tag = NILTAG	'nil -> nil
			keyslot[0] = Long Ptr(1) ; Return Null
		Else	'pointer (needs improvement)
			idx = Int(key) Shr 3
		EndIf
		
		Local hashpart:Byte Ptr = Byte Ptr Ptr(tbl)[2]
		If hashpart
			Local hsize:Int = 1 Shl Int Ptr(hashpart)[-1]
			idx = idx & (hsize - 1)	'apparently this is faster than Mod
			For Local i:Int = idx Until hsize	'naive linear probe
				Local kp:Long Ptr = Long Ptr(hashpart) + 2 * i
				If kp[0] = key Or Int Ptr(kp)[1] = NILTAG
					keyslot[0] = kp ; Return kp + 1
				EndIf
			Next
			For Local i:Int = 0 Until idx	'yep
				Local kp:Long Ptr = Long Ptr(hashpart) + 2 * i
				If kp[0] = key Or Int Ptr(kp)[1] = NILTAG
					keyslot[0] = kp ; Return kp + 1
				EndIf
			Next
		EndIf
		
		Return Null
	End Function
	
	' retrieve a value from a table, or nil
	Function RawGet:Long(tbl:Byte Ptr, key:Long)
		Local keyp:Long Ptr = Null, retp:Long Ptr = GetSlot(tbl, key, Varptr(keyp)), ret:Long
		If retp And (keyp <> Long Ptr(1))
			ret = retp[0]
		Else
			Int Ptr(Varptr(ret))[1] = BlueTypeTag.NANBOX | BlueTypeTag.NIL	'why can't i shift longs?
		EndIf
		Return ret
	End Function
	
	' put a value into a table, resizing it if necessary
	Function RawSet(mem:BlueVMMemory, tbl:Byte Ptr, key:Long, val:Long)
		Local keyp:Long Ptr = Null, valp:Long Ptr = GetSlot(tbl, key, Varptr(keyp))
		Const NILTAG:Int = BlueTypeTag.NANBOX | BlueTypeTag.NIL
		If valp
			mem.Write(valp, val)
			If keyp
				Local hashpart:Int Ptr = Int Ptr Ptr(tbl)[2]
				If Int Ptr(valp)[1] <> NILTAG
					If keyp[0] <> key Then mem.Write(keyp, key) ; hashpart[-2] :+ 1
				Else	'nil value = remove element
					mem.Write(keyp, val) ; hashpart[-2] :- 1
				EndIf
			EndIf
		ElseIf keyp <> Long Ptr(1)
			Resize(mem, tbl, key) ; RawSet mem, tbl, key, val
		EndIf
	End Function
	
	' resize a table (adds space for the extra key, tries to reshuffle integer-keyed elements for optimal space usage)
	Function Resize(mem:BlueVMMemory, tbl:Byte Ptr, key:Long)
		Local hashpart:Byte Ptr = Byte Ptr Ptr(tbl)[2], arraypart:Byte Ptr = Byte Ptr Ptr(tbl)[3]
		Const NILTAG:Int = BlueTypeTag.NANBOX | BlueTypeTag.NIL
		
		Local asize:Int = 0, tsize:Int = 0, numcount:Int[32]
		If arraypart	'compute new array size
			If Int Ptr(arraypart)[1] <> NILTAG Then numcount[0] = 1
			Local cell:Int = 1, cellp2:Int = 2 ^ cell
			For Local i:Int = 1 Until Int Ptr(arraypart)[-1]
				If i >= cellp2 Then cell :+ 1 ; cellp2 = 2 ^ cell
				If Int Ptr(arraypart)[i * 2 + 1] <> NILTAG Then numcount[cell] :+ 1
			Next
		EndIf
		If hashpart
			For Local i:Int = 0 Until Int Ptr(hashpart)[-2]
				Local tag2:Int = Int Ptr(hashpart)[i * 4 + 1]
				If tag2 & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX
					Local d:Double = Double Ptr(hashpart)[i * 2]
					If d = Abs Int(d) Then numcount[Ceil(Log(d + 1) / Log(2))] :+ 1 Else tsize :+ 1
				Else
					tsize :+ 1	'get started on new table size
				EndIf
			Next
		EndIf
		
		Local tag:Int = Int Ptr(Varptr(key))[1]	'add key to the appropriate one
		If tag & BlueTypeTag.NANBOX_CHK <> BlueTypeTag.NANBOX
			Local d:Double = Double Ptr(Varptr(key))[0]
			If d = Abs Int(d) Then numcount[Ceil(Log(d + 1) / Log(2))] :+ 1 Else tsize :+ 1
		Else
			tsize :+ 1
		EndIf
		For Local i:Int = 0 Until 32	'tally up
			If i Then numcount[i] :+ numcount[i - 1]
			If numcount[i] > 2 ^ i / 2 Then asize = 2 ^ i
		Next
		If arraypart <> Null	'complete new table size with any discarded array elements
			For Local i:Int = asize Until Int Ptr(arraypart)[-1]
				If Int Ptr(arraypart)[i * 2 + 1] <> NILTAG Then tsize :+ 1
			Next
		EndIf
		
		Local newarray:Byte Ptr = Null, newtable:Byte Ptr = Null	'allocate and copy
		Local oldasize:Int = 0 ; If arraypart Then oldasize = Int Ptr(arraypart)[-1]
		Local oldtsize:Int = 0 ; If hashpart Then oldtsize = Int Ptr(hashpart)[-1]
		
		If asize <> oldasize
			newarray = mem.AllocObject(asize * 8 + 8, BlueTypeTag.ARR) + 8
			For Local i:Int = 0 Until asize
				Int Ptr(newarray)[i * 2 + 1] = NILTAG
			Next
			Int Ptr(newarray)[-1] = asize ; Byte Ptr Ptr(tbl)[3] = newarray
		ElseIf asize = 0
			Int Ptr(tbl)[3] = 0
		Else
			newarray = arraypart ; arraypart = Null
		EndIf
		If tsize <> oldtsize
			newtable = mem.AllocObject(2 ^ tsize * 16 + 8, BlueTypeTag.HASH)
			For Local i:Int = 0 Until 2 ^ tsize * 2
				Int Ptr(newtable)[i * 2 + 1] = NILTAG
			Next
			Int Ptr(newtable)[-1] = tsize ; Int Ptr(newtable)[-2] = 0
			Byte Ptr Ptr(tbl)[2] = newtable
		ElseIf tsize = 0
			Int Ptr(tbl)[2] = 0
		Else
			newtable = hashpart ; hashpart = Null
		EndIf
		
		If arraypart
			For Local i:Int = 0 Until oldasize	'reinsert values
				Local key:Long ; Double Ptr(Varptr(key))[0] = i
				RawSet mem, tbl, key, Long Ptr(arraypart)[i]
			Next
		EndIf
		If hashpart
			For Local i:Int = 0 Until 2 ^ oldtsize
				RawSet mem, tbl, Long Ptr(hashpart)[i * 2], Long Ptr(hashpart)[i * 2 + 1]
			Next
		EndIf
	End Function
End Type

