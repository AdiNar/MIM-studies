// Przykład kilkukrotnie zagnieżdżonego pattern matchingu

data A() = Aa(Int()) | Ab(A());

a = Ab (Ab (Aa 5));

main = case a of
     |> Ab (Ab (Aa (x))) => x <|