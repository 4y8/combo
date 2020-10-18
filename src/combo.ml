open Fun

type ('a, 'b) parser = 'a list -> ('b * 'a list) option

(* Helper functions *)
let rec explode s =
  match s with
    "" -> []
  | _  ->
    (String.get s 0) ::
    (explode (String.sub s 1 (String.length s - 1)))

let rec inplode s =
  match s with
    [] -> ""
  | hd :: tl ->
    (String.make 1 hd) ^ (inplode tl)

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

let ( <$ ) f p =
  (const <$> return f) <*> p

let ( *> ) p q =
  (id <$ p) <*> q

let ( <* ) p q =
  (const <$> p) <*> q

let (<**>) p1 p2 =
  (fun x f -> f x) <$> p1 <*> p2

let sat f =
  fun s ->
  match s with
    hd :: tl when f hd -> Some (hd, tl)
  | _ -> None

let char c =
  sat ((=) c)

let range l r =
  sat (fun x -> l <= x && x <= r)

let lower =
  range 'a' 'z'

let upper =
  range 'A' 'Z'

let alpha =
  lower <|> upper

let digit =
  range '0' '9'

let any =
  function
    [] -> None
  | hd :: tl -> Some (hd, tl)

let opt default x =
  x <|> return default

let rec many p =
  opt [] (p >>= fun r -> many p >>= fun rs -> return (r :: rs))

let many1 p =
  List.cons <$> p <*> many p

let rec word w =
  match explode w with
    [] -> return []
  | hd :: tl ->
     (List.cons <$> char hd) <*> word (inplode tl)
