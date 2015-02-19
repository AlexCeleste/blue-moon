SuperStrict
Framework pub.lua				'Total Time = 10.546
'Framework zeke.luajit1		'Total Time = 4.156
'Framework zeke.luajit2			'Total Time = 0.443

Import brl.standardio
Import brl.polledinput


Global l:Byte Ptr = luaL_newstate()

luaL_openlibs(l)


If luaL_dofile(l , "sieve.lua") Then 
	Print lua_tostring(l , - 1)
EndIf

WaitKey