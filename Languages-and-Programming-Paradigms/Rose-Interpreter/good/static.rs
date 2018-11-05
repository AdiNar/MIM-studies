// Test na statyczną widoczność zmiennych, dynamiczna dałaby wynik 1000

{Int () -> Int ()} f x = x;
{Int () -> Int ()} g x = f x;
main = let {Int () -> Int ()} f x = 100 * x in g 10