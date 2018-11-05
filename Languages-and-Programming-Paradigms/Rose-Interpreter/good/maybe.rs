// Prosty przykład monady maybe, niestety na razie bez polimorfizmu
data Maybe () = Just (Int()) | Nothing();

{List(Int()) -> Maybe()} first l =
	     case l of
	     	  |> [] => Nothing <|
		  |> (x:_) => Just x <|;

main = first [1,2,3]