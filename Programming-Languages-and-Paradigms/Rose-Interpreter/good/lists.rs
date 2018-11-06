// Przykład deklaracji listy oraz pattern matchingu list.
list = [1,2,3,4,5];

main =
     (case list of
     	 |> (x:(y:_)) => x + y <|) +
     (case list of
         |> [1, _, x, y, z] => x + y + z <|)