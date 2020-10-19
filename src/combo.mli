(* Utils *)

val explode : string -> char list
val inplode : char list -> string

(** Basic type of parsers. *)
type ('a, 'b) parser = 'a list -> ('b * 'a list) option

(** A basic combinator that always succeeds returning the given value. *)
val return : 'a -> ('s, 'a) parser

(** [fail] is a basic combinator which always fails. *)
val fail: ('s, 'a) parser

(** Sequence combinator appliying the result of the second parser to the first
   parser. *)
val ( <*> ) : ('s, 'b -> 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

(** [<**>] is the sequencing operator applying its right-hand side operand to
   the left-hand side one, instead of doing it in the other way. *)
val ( <**> ) : ('s, 'b) parser -> ('s, 'b -> 'a) parser -> ('s, 'a) parser

(** Sequence monad. *)
val ( >>= ) : ('s, 'a) parser -> ('a -> ('s, 'b) parser) -> ('s, 'b) parser

(** Choice combinator trying the first parser, if it works, return the result,
   else return the result of the second parser. *)
val ( <|> ) : ('s, 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser

(** Map combinator applying the result of the given parser to the given
   function, if he succeeds. *)
val ( <$> ) : ('b -> 'a) -> ('s, 'b) parser -> ('s, 'a) parser

(** Map combinator ignoring the right value. *)
val ( <$ ) : ('b -> 'a) -> ('s, 'b) parser -> ('s, 'b -> 'a) parser

(** Sequence combinator ignoring the left value. *)
val ( *> ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b) parser

(** Sequence combinator ignoring the right value. *)
val ( <* ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

(* Basic parsers *)
(** A parser that matches an element satisfying the given predicate. *)
val sat : ('a -> bool) -> ('a, 'a) parser

(** A parser that matches the given character. *)
val char : char -> (char, char) parser

(** A parser that matches a character in the given range, inclusive. *)
val range : char -> char -> (char, char) parser

(** A parser that matches an lowercase character *)
val lower : (char, char) parser

(** A parser that matches an uppercase character *)
val upper : (char, char) parser

(** A parser that matches an alphabet character. *)
val alpha : (char, char) parser

(** [digit] is a parser that matches a digit. *)
val digit : (char, char) parser

(** [space] is a parser that matches a space. *)
val space : (char, char) parser

(** [spaces] is a parser that matches 0 or more spaces. *)
val spaces : (char, char list) parser

(** [any] is a parser that matches anything. *)
val any : ('a, 'a) parser

(** [opt default p] is parser that runs the parser [p] and if it succeeds return
   the result, else, it returns the [default] value given. *)
val opt : 'a -> ('s, 'a) parser -> ('s, 'a) parser

(** [many p] is a parser that runs the parser p 0 or more times and returns all
   the obtained results in a list. *)
val many : ('s, 'a) parser -> ('s, 'a list) parser

(** [many1 p] is a parser that runs the parser p 0 or more times and returns all
   the obtained results in a list. *)
val many1 : ('s, 'a) parser -> ('s, 'a list) parser

(** [word w] is a parser that matches the word [w]. *)
val word : string -> (char, char list) parser 
