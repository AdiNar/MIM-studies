// przykład przesyłania funkcji jako argumentu
{(Int () -> Int ()) -> Int () -> Int ()}
f a b = (a 5) * b;
{Int () -> Int ()} g a = 4 * a;
main = f g 4     