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
--[ [
local x = quux + 2	--assumes existence of test value _ENV.quux
--]]
