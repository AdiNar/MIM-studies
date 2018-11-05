// redeklaracja typów jest zabroniona

data A() = A();

main = let data A() = A() in 10 + 5