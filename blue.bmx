
' Blue Moon
' an implementation of Lua in pure BlitzMax

SuperStrict

'Framework Brl.Blitz
'Import Brl.LinkedList
'Import Brl.Map
'Import Brl.Reflection

Import "bluecompiler.bmx"
Import "bluevm.bmx"

'Const file:String = "tests/checktable.lua"
'Const file:String = "tests/mandelbrot.lua"
'Const file:String = "tests/sieve.lua"
'Const file:String = "tests/attrib.lua"
Const file:String = "test1.lua"

Const outFile:String = "out.lua.so"


Print BlueCompiler.ShowBytecode(file)

Try
	Local code:BlueBinary = BlueCompiler.CompileFileForLoad(file)
	Local vm:BlueVM = New BlueVM
	vm.LoadObjectCode(code)
Catch err:BlueError
	Print err.ToString()
End Try

Print "done."
End

