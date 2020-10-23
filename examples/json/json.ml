open Combo

type jsonVal
  = JNull
  | JBool of bool
  | JNumber of int
  | JString of string
  | JArray of jsonVal list
  | JObject of (string * jsonVal) list
[@@deriving show]

let (%) f g x = f (g x)

let jsonNull = JNull <$ word "null"

let jsonBool = 
  let jbool b = JBool b in
  (jbool % bool_of_string % inplode) <$> (word "true" <|> word "false")

let jsonNumber =
  let jnumber n = JNumber n in
  (jnumber % int_of_string % inplode) <$> (many1 digit)

let string = 
  inplode <$> (char '"' *> many (satisfy ((<>) '"')) <* char '"')

let jsonString =
  let jstring s =  JString s in
  (* Note: no escape sequence in strings. *)
  jstring <$> string

let sepcomma =
  spaces *> char ',' <* spaces

let rec jsonValue s =
  choice [jsonNull; jsonBool; jsonNumber; jsonString; jsonArray; jsonObject] s

and jsonArray s =
  let jarray l = JArray l in
  (jarray
   <$> char '['
    *> spaces
    *> sepBy sepcomma jsonValue
   <*  spaces
   <*  char ']') s

and jsonObject s =
  let pair =
    (fun x y -> x, y)
    <$> string
    <*> spaces
     *> char ':'
     *> spaces
     *> jsonValue
  in
  let jobject l = JObject l in
  (jobject
   <$> char '{'
    *> spaces
    *> sepBy sepcomma pair
   <*  spaces
   <*  char '}') s

let () =
  let s = read_line () in
  match jsonValue (explode s) with
    None -> print_endline "ERROR: bad expression." 
  | Some (n, _) -> (print_endline % show_jsonVal) n
