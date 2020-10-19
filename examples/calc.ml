open Combo
open Fun

let positive = (int_of_string <$> (inplode <$> many digit))
let int  = (opt id ((( * ) (-1)) <$ char '-')) <*> positive 

let plus  = spaces *> ((+) <$ char '+') <* spaces
let minus = spaces *> ((-) <$ char '-') <* spaces

let plus = chainl (plus <|> minus) int

let term = int <|> plus

let _ =
  match plus (explode "-13 + 2") with
    None -> ()
  | Some (n, _) -> print_int n
