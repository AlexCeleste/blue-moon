SuperStrict
Framework pub.lua				'Time Elapsed 44
'Framework zeke.luajit1		'Time Elapsed 5
'Framework zeke.luajit2			'Time Elapsed 2

Import brl.standardio
Import brl.polledinput

Global l:Byte Ptr = luaL_newstate()

luaL_openlibs(l)


If luaL_dofile(l , "mandelbrot.lua") Then 
	Print lua_tostring(l , - 1)
EndIf

'WaitKey