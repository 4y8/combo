open Fun

let (%) f g x = f (g x) 

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

let ( <*> ) p q =
  fun s ->
  match p s with
    None -> None
  | Some (a, s) ->
     match q s with
       None -> None
     | Some (b, s) -> Some (a b, s)

let ( >>= ) p q =
  fun s ->
  match p s with
    None -> None
  | Some (a, s) ->
     q a s

let ( <|> ) p q =
  fun s ->
  match p s with
    None -> q s
  | Some _ as r -> r

let ( <$> ) f p =
  (return f) <*> p

let ( <$ ) f p =
  (const <$> return f) <*> p

let ( *> ) p q =
  (id <$ p) <*> q

let ( <* ) p q =
  (const <$> p) <*> q

let ( <**> ) p q =
  (fun x f -> f x) <$> p <*> q

let opt default x =
  x <|> return default

let ( <??> ) p q =
  p <**> opt id q

let sat f =
  fun s ->
  match s with
    hd :: tl when f hd -> Some (hd, tl)
  | _ -> None

let sym s =
  sat ((=) s)

let rec syms s =
  match s with
    [] -> return []
  | hd :: tl ->
     List.cons <$> sym hd <*> syms tl

let char =
  sym

let word =
  syms % explode

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

let rec many p =
  opt [] (p >>= fun hd -> many p >>= fun tl -> return (hd :: tl))

let many1 p =
  List.cons <$> p <*> many p

let rec appall x =
  function
    [] -> x
  | hd :: tl -> appall (hd x) tl

let chainl op p =
  appall <$> p <*> many (flip <$> op <*> p)

let rec chainr op p =
  p <??> (op >>= fun x -> chainr op p >>= fun y -> return (flip x y))

let choice x = List.fold_right (<|>) x fail

let space =
  char ' '

let spaces =
  many space

let pack l p r =
  syms l *> p <* syms r
