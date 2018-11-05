// Lambda z rekursją nie jest dozwolona, ten sam efekt uzyska się poprzez zdefiniowanie nazwanej funkcji
main = let f = \Int () -> Int () x -> if x == 0 then 1 else x * (f (x-1)) in f 5
