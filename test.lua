--[ [
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
end
--]]
--[[
local y = 2
local x = 0
local function f(x) return x + y end
while x ~= 6 do
	x = f(x)
end--]]
--local a, b, c = 1, 2, 3
--a = 6 ; b = 7 ; c = 8
--[[
local x = x
local x = a.b

goto a
::a::
]]
--local x, y, g
--n = (x + 6) * y / +g .. 8
--[[local x, g
x:f(0)
g(x, 0)]]
--[[
do
	local x = 6
	local function f(y, z, w) return x end
	local function g() return f(x) end
end
]]
--local pr, x = print ; pr()
--[[if x == 0 then
	pr("zero")
else
	pr "one"
end--]
while not (x == 0) do
	pr "two"
	break
end--]]
--x = x and pr
--x = not x or pr
--[[
repeat
	local x
	print "loop"
	break
until x
x = 1
--]]
--[[
do
	local print = print
	for n = x, y do
		print(n)
		break
	end
end
]]
--[[g()
local x, y = 1, 2, 3, ...
function a()
	local x
	return function() return x + y + z end
end]]
--[[
for i,j in g,t,w do
	print(i)
end
do
	local f, s, var = explist
	while true do
		local var_1, var_2, var_n = f(s, var)
		if var_1 == nil then break end
		var = var_1
		print(var_1)
	end
end
]]
--local v1, v2, v3 = 1, 2, 3
--[[
for x = a, b do
	print(x)
end
do
	local var, limit, step = tonumber(a), tonumber(b), tonumber(1)
	if not (var and limit and step) then error() end
	var = var - step
	while true do
	 var = var + step
	 if (step >= 0 and var > limit) or (step < 0 and var < limit) then
	   break
	 end
	 local x = var
	 print(x)
	end
end
]]
--[[
local p = print
p(1, x)
print(p)
]]
--[[
goto b
::a::
local z = 1, 2
local a, b = 1
z = true
x, y, z = 1, 2
x, y = 1, 2, 3
::b::
goto a
]]
--[[
local x
x[1], x[2], x[3] = a, b, c, 5, 6
y[5] = z[6]
]]
--[[local x = {
	1, b = 2; [3] = 3, 4, g()
}]]
--[[
local a, b, c = ...
local a, b, c = f()
a, b, c = f()
a, b, c = ...
]]
--[[
local function all(...) return ... end
return ...
return all(1, 2, 3)
]]
--[[]
local z = {}
local y = { all(7, 8, 9), 7, 8, 9, all(12, 13) }
local x = { 7, 8, 9, x = 6, ... }
]]
