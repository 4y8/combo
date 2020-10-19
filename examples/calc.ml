open Combo

let int = int_of_string <$> (inplode <$> many digit)

let plus = (+) <$> int <* spaces <* char '+' *> spaces <*> int

let term = int <|> plus

let _ =
  match plus (explode "13 + 2") with
    None -> ()
  | Some (n, _) -> print_int n
