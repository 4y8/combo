open Combo
open Fun

let positive = (int_of_string <$> (inplode <$> many digit))
let int  = (opt id ((( * ) (-1)) <$ char '-')) <*> positive 

let op (f, s) = spaces *> (f <$ word s) <* spaces
let anyop l = choice (List.map op l)
let addops = anyop [(+), "+"; (-), "-"]
let mulops = anyop [( * ), "*"]
let ops = List.fold_right chainl [addops; mulops] int 

let _ =
  match ops (explode "13 * 2") with
    None -> ()
  | Some (n, _) -> print_int n
