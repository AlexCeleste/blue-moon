
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

Local code:BlueBinary = BlueCompiler.CompileFileForLoad(file)
Local vm:BlueVM = New BlueVM
Local tl:BlueLuaVal = vm.LoadObjectCode(code)

Local stk:Stack = BlueJIT.BPtoS(vm.mem.stack)
stk.retIP = Null ; stk.prevBase = Null
Local vc:Int = 10, upvars:Int = 1
stk.varp = Long Ptr(Byte Ptr(stk) + BlueJIT.STACKFRAME_INC) + upvars
stk.func = vm.funIndex[0]
stk.argv = Null	'may want to add space?
stk.retv = Null
stk.argc = 0
stk.retc = 0

Print "running..."
Local t:Int = MilliSecs()
Local test:Int(_:Byte Ptr) = stk.func.mcode - BlueJIT.PROLOGUESZ ; test(stk)
t = MilliSecs() - t
Print t
Print "run complete"


Print "done."
End


Rem
Local files:TList = CreateList()
Local as:String, ld:String, showHelp:Int, output:String = "a.out", doAssemble:Int = 1
Local keepAsm:Int, makeExe:Int = 1, asOpts:String, ldOpts:String, showAST:Int, showVersion:Int

?MacOS
as = "clang -m32 -c " ; ld = "clang -Wl,-no_pie -m32 -read_only_relocs suppress "
?Not MacOS
as = "gcc -m32 -c " ; ld = "gcc -m32 "
?

For Local a:Int = 1 Until AppArgs.Length
	Select AppArgs[a]
		Case "-?", "--help"
			showHelp = 1
		Case "-v"
			showVersion = 1
		Case "-o"
			If makeExe = 0
				Print "warning: -c and -o are mutually exclusive; -c is overruling -o"
			Else
				a :+ 1 ; output = AppArgs[a] ; makeExe = 2
			EndIf
		Case "-c"
			If makeExe = 2
				Print "warning: -o and -c are mutually exclusive; -o is overruling -c"
			Else
				doAssemble = 1 ; makeExe = 0
			EndIf
		Case "-s"
			keepAsm = 1
		Case "-S"
			keepAsm = 1 ; doAssemble = 0 ; makeExe = 0
		Case "--as"
			a :+ 1 ; as = AppArgs[a] + " "
		Case "--ld"
			a :+ 1 ; ld = AppArgs[a] + " "
		Case "--as-opt"
			a :+ 1 ; asOpts :+ AppArgs[a] + " "
		Case "--ld-opt"
			a :+ 1 ; ldOpts :+ AppArgs[a] + " "
		Case "--tree"
			showAST = 1 ; makeExe = 0
		Case "-w"     ; YBCodeGen.SetWarningLevel 0
		Case "--werr" ; YBCodeGen.SetWarningLevel 2
		Case "--warn" ; YBCodeGen.SetWarningLevel 1
		Default
			files.AddLast AppArgs[a]
	End Select
Next
If AppArgs.Length = 1 Then Print "ybc: no input files" ; End

If showVersion Then DisplayVersion
If showHelp Then DisplayHelp

?Win32
Local rm:String = "del /Q "
?Not Win32
Local rm:String = "rm "
?

Local allOFiles:String = ""
For Local file:String = EachIn files
	Try
		Local tree:TParseNode = YBParseFile(file)
		If showAST
			Print tree.ToString()
		Else
			YBCodeGen.Build tree
			YBAssembler.Emit file + ".s", YBCodeGen.syms, YBCodeGen.funs, YBCodeGen.vars, YBCodeGen.strs
			If doAssemble
				system_(as + asOpts + file + ".s -o " + file + ".o")
				allOFiles :+ file + ".o "
			EndIf
			If Not keepAsm
				system_ rm + file + ".s"
			EndIf
		EndIf
	Catch e:Object
		Print "Compile error:~n    " + e.ToString()
		Print "Compilation halted."
		?Debug
		Throw e
		?
		End
	End Try
Next

?Linux
Local bLib:String = "b-lib-linux"
?Not Linux
Local bLib:String = "b-lib"
?
If makeExe
	If Not FileType("b-lib.o") Then system_ as + bLib + ".s -o b-lib.o"
	system_(ld + ldOpts + "-o " + output + " " + allOFiles + " b-lib.o")
	system_(rm + allOFiles)
EndIf

Print "done."
End
End Rem
Rem
Function DisplayVersion()
	Print "Shadow SIMD Compiler: version 0.0"
End Function

Function DisplayHelp()
	Print "OVERVIEW: ybc compiler for B~n"
	Print "USAGE: ybc [options] <files>~n"
	Print "OPTIONS:~n"
	Print "  -?, --help  Display this message"
	Print "  -v          Show the compiler version"
	Print "  -o          Set the name of the output executable (default 'a.out')"
	Print "  -c          Produce separate .o files instead of an executable"
	Print "  -s          Keep text assembly .s files"
	Print "  -S          Only produce text assembly, do not assemble binaries"
	Print "  --as        Set the command to use as the assembler"
	Print "  --ld        Set the command to use as the linker"
	Print "  --as-opt    Add an option to pass to the assembler (can repeat)"
	Print "  --ld-opt    Add an option to pass to the linker (can repeat)"
	Print "  --tree      Display the AST of the program source instead of compiling"
	Print "  -w          Silence warnings"
	Print "  --werr      Convert warnings to errors"
	Print "  --warn      Notify but do not halt on warnings (default)"
End Function
End Rem
