(** Basic type of parsers. *)
type ('a, 'b) parser = 'a list -> ('b * 'a list) option

(** A basic combinator that always succeeds returning the given value. *)
val return : 'a -> ('s, 'a) parser

(** A basic combinator that always fails. *)
val fail: ('s, 'a) parser

(** Sequence combinator appliying the result of the second parser to the first
   parser.*)
val ( <*> ) : ('s, 'b -> 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

(** Sequence monad. *)
val ( >>= ) : ('s, 'a) parser -> ('a -> ('s, 'b) parser) -> ('s, 'b) parser

(** Choice combinator trying the first parser, if it works, return the result,
   else return the result of the second parser. *)
val ( <|> ) : ('s, 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser

(** Map combinator applying the result of the given parser to the given
   function, if he succeeds. *)
val ( <$> ) : ('b -> 'a) -> ('s, 'b) parser -> ('s, 'a) parser

(** Sequence combinator ignoring the left value. *)
val ( *> ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b) parser

(** Sequence combinator ignoring the right value. *)
val ( <* ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

(* Basic parsers *)
(** A parser that matches an element satisfying the given predicate. *)
val sat : ('a -> bool) -> ('a, 'a) parser

(** A parser that matches the given character. *)
val char : char -> (char, char) parser

(** A parser that matches an alphabet character. *)
val alpha : (char, char) parser

(** A parser that matches a digit. *)
val digit : (char, char) parser

(** A parser that matches anything. *)
val any : ('a, 'a) parser
