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

let ( <&> ) p f =
  f <$> p 

let ( <$ ) f p =
  (const <$> return f) <*> p

let ( $> ) p f =
  f <$ p

let ( *> ) p q =
  (id <$ p) <*> q

let ( <* ) p q =
  (const <$> p) <*> q

let ( *>| ) p q =
  (id <$ p) <*>| q

let ( <*| ) p q =
  (const <$> p) <*>| q

let ( <**> ) p q =
  (fun x f -> f x) <$> p <*> q

let ( <?> ) p err =
  fun s ->
  match p s with
    None -> raise err
  | x -> x

let opt default x =
  x <|> return default

let ( <??> ) p q =
  p <**> opt id q

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

let between op p cl =
  op *> p <* cl

let sepBy1 sep p =
  (List.cons) <$> p <*> many (sep *> p)

let sepBy sep p =
  opt [] (sepBy1 sep p)

let endBy1 sep p =
  many1 (p <* sep)

let endBy sep p =
  opt [] (endBy1 sep p)

let sepEndBy1 sep p =
  (sepBy1 sep p) <**> (opt id (id <$ sep))

let sepEndBy sep p =
  opt [] (sepEndBy1 sep p)

let satisfy p =
  fun s ->
  match s with
    hd :: tl when p hd -> Some (hd, tl)
  | _ -> None

let sym s =
  satisfy ((=) s)

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
  satisfy (fun x -> l <= x && x <= r)

let lower =
  range 'a' 'z'

let upper =
  range 'A' 'Z'

let letter =
  lower <|> upper

let digit =
  range '0' '9'

let alphaNum =
  letter <|> digit

let octDigit =
  range '0' '7'

let hexDigit =
  digit <|> range 'a' 'f' <|> range 'A' 'F'

let any =
  function
    [] -> None
  | hd :: tl -> Some (hd, tl)

let space =
  char ' '

let spaces =
  many space

let tab =
  char '\t'

let newline =
  char '\n'

let pack l p r =
  between (syms l) p (syms r)

let packs l p r =
  between (word l) p (word r)

let oneOf l =
  choice (List.map sym l)
