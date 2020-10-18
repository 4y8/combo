open Fun

type ('a, 'b) parser = 'a list -> ('b * 'a list) option

let return a =
  fun s -> Some (a, s)

let fail =
  fun _ -> None

let ( <*> ) p1 p2 =
  fun s ->
  match p1 s with
    None -> None
  | Some (a, s) ->
     match p2 s with
       None -> None
     | Some (b, s) -> Some (a b, s)

let ( >>= ) p1 p2 =
  fun s ->
  match p1 s with
    None -> None
  | Some (a, s) ->
     p2 a s

let ( <|> ) p1 p2 =
  fun s ->
  match p1 s with
    None -> p2 s
  | Some _ as r -> r

let ( <$> ) f p =
  (return f) <*> p

let ( *> ) p q =
  ((flip const) <$> p) <*> q

let ( <* ) p q =
  (const <$> p) <*> q

let sat f =
  fun s ->
  match s with
    hd :: tl when f hd -> Some (hd, tl)
  | _ -> None

let char c =
  sat ((=) c)

let alpha =
  sat (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

let digit =
  sat (function '0' .. '9' -> true | _ -> false)

let any s =
  match s with
    [] -> None
  | hd :: tl -> Some (hd, tl)
 
let _ = print_endline "Hello"
