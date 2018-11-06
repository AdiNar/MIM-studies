{List (Int()) -> Int()} listSum l =
      case l of
      	  |> [] => 0 <|
	  |> (x:xs) => x + (listSum xs) <|;

main = listSum [1,2,3,4,5]