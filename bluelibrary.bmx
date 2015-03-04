
' Blue Moon
' Basic Lua library

' this is the minimal "standard library", designed to be loaded into a VM on start
' most of this doesn't use the convenience interface so as to keep overhead down

' for now this is an Include rather than Import lib due to recursive dependency
' we'll fix that later

Rem
SuperStrict

Import "blueerror.bmx"
Import "bluevm.bmx"
Import "bluetable.bmx"
End Rem

Type BlueBasicLibrary
	Function _Load(_ENV:Byte Ptr)
	End Function
	
	Function getmetatable()
	End Function
	Function setmetatable()
	End Function
	
End Type

