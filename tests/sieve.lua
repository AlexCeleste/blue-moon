--jit.off()

function sieve(n)
  x = {}
  iter = 0
  repeat
    x[1] = 0
    i = 2
    repeat
      x[i] = 1
      i = i + 1
    until i > n
    p = 2
    while(p * p <= n) do
      j = p
      while(j <= n) do
        x[j] = 0
        j = j + p
      end
      repeat
        p = p + 1
      until x[p] == 1
    end
    iter = iter + 1
  until iter == 101
end

print("Sieve of Eratosthenes - Lua Benchmark test 0.00001")
print("Start testing .....")
start = os.clock()
sieve(100000)
stop = os.clock()
print("Done!")
print("Total Time = "..(stop - start))
--Pub.lua time=13.218
--LuaJIT  time=4.933
--LuaJIT2 time=0.397

