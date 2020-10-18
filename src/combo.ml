open Fun

type ('a, 'b) parser = 'a list -> ('b * 'a list) option

let return a =
  fun s -> Some (a, s)

let fail =
  const None

let (<*>) p1 p2 =
  fun s ->
  match p1 s with
    None -> None
  | Some (a, s) ->
     match p2 s with
       None -> None
     | Some (b, s) -> Some (a b, s)

let (<|>) p1 p2 =
  fun s ->
  match p1 s with
    None -> p2 s
  | Some _ as r -> r

let (<$>) f p =
  (return f) <*> p

let (>>) p q =
  ((flip const) <$> p) <*> q

let (<<) p q =
  (const <$> p) <*> q

let _ = print_endline "Hello"
