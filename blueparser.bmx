
' Blue Moon
' parser and lexer

SuperStrict

Import "TMeta.bmx"	'Get this here: http://www.blitzbasic.com/codearcs/codearcs.php?code=3090
Import "blueerror.bmx"


Function BlueParseFile:TParseNode(file:String)
	If _parser = Null Then _parser = New BlueParser
	If _lexer = Null Then _lexer = BlueLexer.Get()
	
	If FileType(file) = 0 Then Throw BlueFileError.Make("file '" + file + "' does not exist") ..
	  ElseIf FileType(file) <> 1 Then Throw BlueFileError.Make("'" + file + "' is not a valid source file")
	Local toks:TToken[] = _lexer.ScanFile(file)
	
	For Local i:Int = 0 Until toks.Length	'Normalize token stream
		If toks[i].ttype = "sconst" Then FilterString(toks[i])
	Next
	Return _parser.Parse(toks, True)
End Function

Function BlueParseString:TParseNode(s:String)
	If _parser = Null Then _parser = New BlueParser
	If _lexer = Null Then _lexer = BlueLexer.Get()
	
	Local toks:TToken[] = _lexer.ScanString(s)
	
	For Local i:Int = 0 Until toks.Length	'Normalize token stream
		If toks[i].ttype = "sconst" Then FilterString(toks[i])
	Next
	Return _parser.Parse(toks, True)
End Function


Private

Global _parser:BlueParser, _lexer:TLexer

Type BlueParser Extends TMetaParser Final
	
	Field grammar:TMap {..
		Chunk = "! Block"..
		Block = "Statement* : @stmts"..
	..
		Statement = "%semicolon | Assgt | FuncCall | Label | %break | GotoS | DoBlk | WhileS | RepeatS | IfS | ForN | ForIn | FunDecl | LocalFun | LocalVar | ReturnS"..
			Assgt = "VarList %equals ! ExpList : @vars - @vals"..
			FuncCall = "PostfixExp"..
			GotoS = "%goto %!name : - @dst"..
			DoBlk = "%do ! Block %!end : - @ -"..
			WhileS = "%while ! Expr DoBlk : - @test @block"..
			RepeatS = "%repeat ! Block %!until Expr : - @block - @test"..
			IfS = "%if ! Expr %!then Block ElseIfS* ElseS? %!end : - @test - @then @elif @else -"..
				ElseIfS = "%elseif ! Expr %!then Block : - @test - @then"..
				ElseS = "%else ! Block : - @ ^"..
			ForN = "%for %name %equals ! Expr %!comma Expr StepS? DoBlk : - @var - @from - @to @step @block"..
				StepS = "%comma Expr : - @ ^"..
			ForIn = "%for NameList ! %in ExpList DoBlk : - @vars - @vals @block"..
			FunDecl = "%function ! FuncName FuncBody : - @name @body"..
			LocalFun = "%local %function ! %name FuncBody : - - @name @body"..
			LocalVar = "%local ! NameList VarVals? : - @names @vals"..
				VarVals = "%equals ExpList : - @ ^"..
			ReturnS = "%return ExpList? %semicolon? : - @vals -"..
	..
		Label = "%dblcolon %!name %!dblcolon : - @name -"..
		FuncName = "%name (%dot %!name)* (%colon %!name)? : @n @path @method"..
		VarList = "SubVarList* VarL : @ < ^"..
			SubVarList = "VarL %comma : @ - ^"..
			VarL = "PostfixExp | %name"..
		NameList = "SubNameList* %name : @ < ^"..
			SubNameList = "%name %comma : @ - ^"..
		ExpList = "SubExpList* Expr : @ < ^"..
			SubExpList = "Expr %comma : @ - ^"..
	..
		Expr = "AndExpr (%kor AndExpr)* : @L @R ^"..
		AndExpr = "RelExpr (%kand RelExpr)* : @L @R ^"..
		RelExpr = "CatExpr ((%lt | %gt | %leq | %geq | %neq | %eq) CatExpr)* : @L @R ^"..
		CatExpr = "SumExpr (%dotdot CatExpr)? : @L @R ^"..
		SumExpr = "MulExpr ((%plus | %minus) MulExpr)* : @L @R ^"..
		MulExpr = "UnaryExpr ((%mul | %div | %kmod) UnaryExpr)* : @L @R ^"..
		UnaryExpr = "(%knot | %klen | %plus | %minus)* PowExpr : @op @arg ^"..
		PowExpr = "Atomic (%pow PowExpr)? : @L @R ^"..
	..
		Atomic = "PostfixExp | Atom"..
		PostfixExp = "(%name | ParenExp) (MArgs | FArgs | FieldGet | ElemGet | TableCons | %sconst)* : @F @A ^"..
			MArgs = "%colon %!name FArgs : - @meth @args"..
			FArgs = "%lparen ! ExpList? %!rparen : - @args -"..
			FieldGet = "%dot %!name : - @fld"..
			ElemGet = "%lbracket ! Expr %!rbracket : - @elem -"..
			ParenExp = "%lparen ! Expr %!rparen : - @ - ^"..
		Atom = "%nil | %bconst | Number | %sconst | %dotdotdot | Lambda | TableCons"..
			Number = "%dconst | %hconst | %hfconst"..
			Lambda = "%function FuncBody : - @ ^"..
	..
		FuncBody = "ParList Block %!end : @params @block -"..
			ParList = "%lparen ! (NameList | (SubNameList* %dotdotdot))? %!rparen : - @p -"..
	..
		TableCons = "%lbrace ! FieldList? %!rbrace : - @fields -"..
			FieldList = "FieldS (FieldSep FieldS)* FieldSep? : @ @ -"..
			FieldS = "FieldIndex | FieldKey | Expr"..
				FieldIndex = "%lbracket ! Expr %!rbracket %!equals Expr : - @key - - @val"..
				FieldKey = "%name %equals ! Expr : @key - @val"..
			FieldSep = "%comma | %semicolon"..
	}
	
End Type

Type BlueLexer Final
	Function Get:TLexer()
		Global Store(_:TLexer) = TLexAction.Store, Discard(_:TLexer) = TLexAction.Discard
		
		Global l:TLexer = TLexer.withRules([..
			R("[0-9]+", Store, "dconst", "0123456789"),..	'Numbers
			R("0[xX][0-9a-fA-F]+", Store, "hconst"),..
			R("[0-9]+\.[0-9]+([eE]-?[0-9]+)?", Store, "dconst", "0123456789"),..
			R("[0-9]+[eE]-?[0-9]+", Store, "dconst", "0123456789"),..
			R("0[xX][0-9a-fA-F]+\.[0-9a-fA-F]+([pP]-?[0-9]+)?", Store, "hfconst"),..
		..
			R("(~q|\')([^\n]|\\\R|\\\'|\\\~q)*?\1", Store, "sconst", "'~q"),..	'Strings
			R("(?s)\[(=*)\[.*?\]\1\]", Store, "sconst", "["),..
		..
			R("--[^\n]*", Discard),..			'Line comment
			R("(?s)--\[(=*)\[.*?\]\1\]", Discard, "", "-"),..		'Long comment
		..
			S("and", Store, "kand"),..	'Keywords
			S("break", Store),..
			S("do", Store),..
			S("else", Store),..
			S("elseif", Store),..
			S("end", Store),..
			S("false", Store, "bconst"),..
			S("for", Store),..
			S("function", Store),..
			S("goto", Store),..
			S("if", Store),..
			S("in", Store),..
			S("local", Store),..
			S("nil", Store),..
			S("not", Store, "knot"),..
			S("or", Store, "kor"),..
			S("repeat", Store),..
			S("return", Store),..
			S("then", Store),..
			S("true", Store, "bconst"),..
			S("until", Store),..
			S("while", Store),..
		..
			S("+",  Store, "plus"),..		'Operators
			S("-", Store, "minus"),..
			S("*", Store, "mul"),..
			S("/", Store, "div"),..
			S("%", Store, "kmod"),..
			S("^", Store, "pow"),..
			S("#", Store, "klen"),..
			S("==", Store, "eq"),..
			S("~~=", Store, "neq"),..
			S("<=", Store, "leq"),..
			S(">=", Store, "geq"),..
			S("<", Store, "lt"),..
			S(">", Store, "gt"),..
			S("=", Store, "equals"),..
			S("(", Store, "lparen"),..
			S(")", Store, "rparen"),..
			S("{", Store, "lbrace"),..
			S("}", Store, "rbrace"),..
			S("[", Store, "lbracket"),..
			S("]", Store, "rbracket"),..
			S("::", Store, "dblcolon"),..
			S(";", Store, "semicolon"),..
			S(":", Store, "colon"),..
			S(",", Store, "comma"),..
			S(".", Store, "dot"),..
			S("..", Store, "dotdot"),..
			S("...", Store, "dotdotdot"),..
		..
			R("[a-zA-Z_][a-zA-Z0-9_]*", Store, "name", "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"),..
		..
			R("[^[:space:]]", TLexAction.Error)..		'Raise an error over any other printable character
		])
		l.SetCaseSensitivity True
		
		Return l
		Function R:TLexRule(r:String, a(l:TLexer), res:String = "", ic:String = "")
			Return TLexRule.Create(r, a, res, "", ic)
		End Function
		Function S:TLexRule(r:String, a(l:TLexer), res:String = "")
			Return TLexRule.CreateSimple(r, a, res)
		End Function
	End Function
End Type

Function FilterString(t:TToken)
	Local s:String = t.value
	s = s.Replace("~r~n", "~n")
	s = s.Replace("~n~r", "~n")	'?
	s = s.Replace("~r", "~n")
	If s[0] = "["[0]
		Local c:Int = 1
		Repeat ; c :+ 1 ; Until s[c - 1] = "["[0]
		s = s[c..s.Length - c]
		If s[0] = 10 Then s = s[1..]
	Else
		s = s[1..s.Length - 1]
		s = s.Replace("\~n", "~n")
		s = s.Replace("\a", Chr(7))
		s = s.Replace("\b", Chr(8))
		s = s.Replace("\f", Chr(12))
		s = s.Replace("\n", Chr(10))
		s = s.Replace("\r", Chr(13))
		s = s.Replace("\t", Chr(9))
		s = s.Replace("\v", Chr(11))
		s = s.Replace("\'", "'")
		s = s.Replace("\~q", "~q")
		Local c:Int = 0
		While c < s.Length - 1
			If s[c] = "\"[0]
				If s[c + 1] = "x"[0]
					Function hexDigit:Int(d:Int, t:TToken)
						Select True
							Case d >= 48 And d <= 57 ; Return d - 48
							Case d >= 65 And d <= 70 ; Return d - 65
							Case d >= 97 And d <= 102 ; Return d - 97
						End Select
						Throw BlueCompileError.Make(t, "invalid hex digit '" + Chr(d) + "' in character escape code")
					End Function
					If c + 3 >= s.Length Then Throw BlueCompileError.Make(t, "incomplete character escape in string")
					s = s.Replace(s[c..c + 4], Chr(16 * hexDigit(s[c + 2], t) + hexDigit(s[c + 3], t)))
				ElseIf s[c + 1] >= 48 And s[c + 1] <= 57
					Local ch:Int = Int(s[c + 1..c + 4])
					s = s.Replace(s[c..c + 1 + String(ch).Length], Chr(ch))
				ElseIf s[c + 1] = "z"[0]
					Local d:Int = c + 2
					While s[d] <= 32 ; d :+ 1 ; Wend
					s = s[0..c] + s[d..]
				ElseIf s[c + 1] = "\"[0]
					s = s[0..c] + s[c + 1..] ; c :+ 1
				Else
					Throw BlueCompileError.Make(t, "invalid escape character '" + Chr(s[c + 1]) + "' in string")
				EndIf
			EndIf
			c :+ 1
		Wend
	EndIf
	t.value = s
End Function


Public

