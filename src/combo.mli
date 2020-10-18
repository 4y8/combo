type ('a, 'b) parser = 'a list -> ('b * 'a list) option

val return : 'a -> ('s, 'a) parser
(** Basic parser that always fails. *)
val fail: ('s, 'a) parser
(** Sequence combinator appliying the result of the parser p2 to the parser p1.*)
val ( <*> ) : ('s, 'b -> 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser
(** Sequence monad. *)
val ( >>= ) : ('s, 'a) parser -> ('a -> ('s, 'b) parser) -> ('s, 'b) parser
(** Choice combinator trying the parser p1, if it works, return the result, else
    return the parser p2. *)
val ( <|> ) : ('s, 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser
val ( <$> ) : ('b -> 'a) -> ('s, 'b) parser -> ('s, 'a) parser
val ( *> ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b) parser
val ( <* ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser
