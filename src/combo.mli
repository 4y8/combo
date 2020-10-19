(* Utils ***********************************************************************)

(** [explode s] turns the string [s] into a list of characters. *)
val explode : string -> char list

(** [inplode l] turns the list of characters [l] into a string. *)
val inplode : char list -> string

(** [parser] is the type of parsers. *)
type ('a, 'b) parser = 'a list -> ('b * 'a list) option

(* Basic combinators ***********************************************************)

(** [return a] is a basic combinator that always succeeds returning the value
   [a]. *)
val return : 'a -> ('s, 'a) parser

(** [fail] is a basic combinator which always fails. *)
val fail: ('s, 'a) parser

(** [p <*> q] is the sequence combinator appliying the result of parser [p] to
   the parser [q]. *)
val ( <*> ) : ('s, 'b -> 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

(** [p <**> q] is the sequence combinator applying the result of parser [q] to
   the parser [p], it is the same as [<*>] but in the other way. *)
val ( <**> ) : ('s, 'b) parser -> ('s, 'b -> 'a) parser -> ('s, 'a) parser

(** [<??>] is the reverse sequencing operator but which doesn't modify the first
   result if the second one failed. *)
val ( <??> ) : ('s, 'a) parser -> ('s, 'a -> 'a) parser -> ('s, 'a) parser

(** Sequence monad. *)
val ( >>= ) : ('s, 'a) parser -> ('a -> ('s, 'b) parser) -> ('s, 'b) parser

(** [p <|> q] is the choice combinator trying the parser [p], if it works,
   returns the result, else return the result of the parser [q]. *)
val ( <|> ) : ('s, 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser

(** [f <$> p] is the map combinator applying the function [f] the witness
   returned by the parser [p], if he succeeds. *)
val ( <$> ) : ('b -> 'a) -> ('s, 'b) parser -> ('s, 'a) parser

(** [f <$ p] is the map combinator ignoring the value returned by the parser
   [p]. *)
val ( <$ ) : ('b -> 'a) -> ('s, 'c) parser -> ('s, 'b -> 'a) parser

(** [p *> q] is the sequence combinator but ignores value returned by the parser
   [p], it's the missing bracket. *)
val ( *> ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b) parser

(** [p <* q] is the sequence combinator but ignores value returned by the parser
   [q], it's the missing bracket. *)
val ( <* ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

(** [choice l] is a combinator that turns the list of parser [l] into a single
   one which will match one of them. *)
val choice : ('s, 'a) parser list -> ('s, 'a) parser

(** [seq l] is a combinator that turns a list of parser [l] into a single one
   which will match all of them and return the result in a list. *)
val seq : ('s, 'a) parser list -> ('s, 'a list) parser

(* Basic parsers ***************************************************************)

(** [sat p] is a parser that matches an element satisfying the predicate [p]. *)
val sat : ('a -> bool) -> ('a, 'a) parser

(** [any] is a parser that matches anything. *)
val any : ('a, 'a) parser

(** [opt default p] is parser that runs the parser [p] and if it succeeds return
   the result, else, it returns the [default] value given. *)
val opt : 'a -> ('s, 'a) parser -> ('s, 'a) parser

(** [many p] is a parser that runs the parser [p] 0 or more times and returns
   all the obtained results in a list. *)
val many : ('s, 'a) parser -> ('s, 'a list) parser

(** [many1 p] is a parser that runs the parser [p] 1 or more times and returns
   all the obtained results in a list. *)
val many1 : ('s, 'a) parser -> ('s, 'a list) parser

(** [chainl op p] is a parser that parses the operand [p], as left-associative,
   separated by the separator [op]. *)
val chainl : ('s, 'a -> 'a -> 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser

(** [chainr op p] is a parser that parses the operand [p], as right-associative,
   separated by the separator [op]. *)
val chainr : ('s, 'a -> 'a -> 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser

(** [sym s] is a parser that matches the symbol [s]. *)
val sym : 'a -> ('a, 'a) parser

(** [syms s] is a parser that matches the list of symbol [s]. *)
val syms : 'a list -> ('a, 'a list) parser

(** A parser that matches the given character. *)
val char : char -> (char, char) parser

(** [word w] is a parser that matches the string [w]. *)
val word : string -> (char, char list) parser 

(** [range l r] is a parser that matches a character between the characters [l]
   and [r] included. *)
val range : char -> char -> (char, char) parser

(** [lower] is a parser that matches a lowercase character *)
val lower : (char, char) parser

(** [upper] is a parser that matches an uppercase character *)
val upper : (char, char) parser

(** [alpha] is a parser that matches an alphabet character. *)
val alpha : (char, char) parser

(** [digit] is a parser that matches a digit. *)
val digit : (char, char) parser

(** [space] is a parser that matches a space. *)
val space : (char, char) parser

(** [spaces] is a parser that matches 0 or more spaces. *)
val spaces : (char, char list) parser

(** [pack l p r] is a parser that matches the parser [p] between the symbols [l]
   and [r]. *)
val pack : 's list -> ('s, 'a) parser -> 's list -> ('s, 'a) parser

(** [packs l p r] is a parser that matches the parser [p] between the strings
   [l] and [r]. *)
val packs : string -> (char, 'a) parser -> string -> (char, 'a) parser
