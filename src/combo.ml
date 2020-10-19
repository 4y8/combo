open Fun

type ('a, 'b) parser = 'a list -> ('b * 'a list) option

(* Helper functions *)

let (%) f g x = f (g x) 

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

let ( <*>| ) p q =
  fun s ->
  match p s with
    None -> None
  | Some (a, s) ->
     match (Lazy.force q) s with
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

let ( <|>| ) p q =
  fun s ->
  match p s with
    None -> (Lazy.force q) s
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

let sat p =
  fun s ->
  match s with
    hd :: tl when p hd -> Some (hd, tl)
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
  opt [] (List.cons <$> p <*>| (lazy (many p)))

let many1 p =
  List.cons <$> p <*> many p

let rec appall x =
  function
    [] -> x
  | hd :: tl -> appall (hd x) tl

let chainl1 op p =
  appall <$> p <*> many (flip <$> op <*> p)

let chainl op p default =
  opt default (chainl1 op p)

let rec chainr1 op p =
  p <??> (flip <$> op <*>| lazy (chainr1 op p)) 

let chainr op p default =
  opt default (chainr1 op p)

let choice l =
  List.fold_right (<|>) l fail

let rec seq =
  function
    [] -> return []
  | hd :: tl -> List.cons <$> hd <*> seq tl

let space =
  char ' '

let spaces =
  many space

let pack l p r =
  syms l *> p <* syms r

let packs l p r =
  word l *> p <* word r
