
' Blue Moon
' general error messages

SuperStrict

Import "TMeta.bmx"	'for the compile errors

Type BlueError Abstract
	Field msg:String
	Method ToString:String()
		Return "Blue Moon: " + msg
	End Method
End Type

Type BlueFileError Extends BlueError
	Function Make:BlueFileError(msg:String)
		Local e:BlueFileError = New Self
		e.msg = "error loading file; " + msg
		Return e
	End Function
End Type

Type BlueCompileError Extends BlueError
	Function Make:BlueCompileError(t:TToken, msg:String)
		Local e:BlueCompileError = New Self
		e.msg = "error in " + t.file + " at line " + t.l + ", col " + t.c + ":  " + msg
		Return e
	End Function
End Type

Type BlueAssemblyError Extends BlueError
	Function Make:BlueAssemblyError(file:String, msg:String)
		Local e:BlueAssemblyError = New Self
		e.msg = "error in " + file + ":  " + msg
		Return e
	End Function
End Type

Type BlueInterpretError Extends BlueError
	Function Make:BlueInterpretError(msg:String)
		Local e:BlueInterpretError = New Self
		e.msg = "VM error:  " + msg
		Return e
	End Function
End Type

