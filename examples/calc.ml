open Combo
open Fun

let positive = (int_of_string <$> (inplode <$> many1 digit))
let int = (opt id ((( * ) (-1)) <$ char '-')) <*> positive 

let op (f, s) = spaces *> (f <$ word s) <* spaces
let anyop l = choice (List.map op l)
let addops = anyop [(+), "+"; (-), "-"]
let mulops = anyop [( * ), "*"]
let rec expr s =
  List.fold_right chainl [addops; mulops] (int <|> packs "(" expr ")")  s

let () =
  match expr (explode "(13 + 2) * 2") with
    None -> ()
  | Some (n, _) -> print_int n
