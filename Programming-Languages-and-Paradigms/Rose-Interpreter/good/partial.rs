// przykład częściowej aplikacji
{Int() -> Int() -> Int()} f a b = a + b;
{(Int() -> Int() -> Int()) -> Int() -> (Int() -> Int())} g x y = x y;
main = (g f 1) 2
