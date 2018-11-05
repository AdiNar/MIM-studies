{Int () -> Int ()} fibo x = if x == 0 then 1 else x * (fibo (x-1));

main = fibo 10
