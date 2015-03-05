-- extremely basic execution tests

--[[
local x = 0
--local y = 600000
while x ~= 10000000 do
	x = x + 1
end--]]
--[[
local function f(x) return x + 1 end
local x = 0
while x ~= 10000000 do
	x = f(x)
end--]]
--[[
local y = 2
local x = 0
local function f(x) return x + y end
while x ~= 8 do--10000000 do
	x = f(x)
end--]]
--function f(x) return function() return x end end
--[[
local x = {}
x[0] = 6 ; x[1] = 7 ; x[2] = 8 ; x[3] = 9 ; x.a = 10
local y = 0 ; while y ~= 4 do
	x[y] = x[y] + 1
	x.a = x.a + 1
	y = y + 1
end--]]
--[[
local t = { v = 2 }
function t.f(self2) return self2['v'] end  -- fakeself; test normal calls first
local y = 0 ; while y ~= 8 do
	y = y + t['f'](t)
end--]]
--[[
local t = { v = 2 }
function t:f() return self['v'] end
local y = 0 ; while y ~= 8 do
	y = y + t:f()
end--]]
--[[
local x = quux + 2	--assumes existence of test value _ENV.quux and _ENV.pr (i.e. won't work in release)
pr "hello from Lua!"
local y = 3 + addtriple(12, 14, 19)	--test values _ENV.addtriple and _ENV.ret3
local a, b, c = ret3()
local z = a + b + c
--]]
--[[
print "hello from Lua!"	--real library print
print("foo", "bar")
print(6, print)
print {1}
local x = { a = 12 }
print(rawget(x, 'a'))
rawset(x, 'b', 17)
print(x.b)
print(_VERSION)
--]]
--[ [
for i = 1, 5 do
	print(i)
end
for i = 1, 10, 2 do
	print(i)
end
local x = "foo".."bar"
print(x.."baz")
--]]
local x = #{1, 2, 3}
