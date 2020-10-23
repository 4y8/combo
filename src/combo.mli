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

(** [p <&> f] is the flipped map combinator applying the function [f] the
   witness returned by the parser [p], if he succeeds. *)
val ( <&> ) : ('s, 'b) parser -> ('b -> 'a) -> ('s, 'a) parser

(** [f <$ p] is the map combinator ignoring the value returned by the parser
   [p]. *)
val ( <$ ) : 'a -> ('s, 'b) parser -> ('s, 'a) parser

(** [p $> f] is the reverse map combinator ignoring the value returned by the parser
   [p]. *)
val ( $> ) : ('s, 'a) parser -> 'b -> ('s, 'b) parser

(** [p *> q] is the sequence combinator but ignores value returned by the parser
   [p], it's the missing bracket. *)
val ( *> ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b) parser

(** [p <* q] is the sequence combinator but ignores value returned by the parser
   [q], it's the missing bracket. *)
val ( <* ) : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser

(** [p <?> err] is the error combinator raising the error err if the parser [p]
   failed. *)
val ( <?> ) : ('s, 'a) parser -> exn -> ('s, 'a) parser

(** [choice l] is a combinator that turns the list of parser [l] into a single
   one which will match one of them. *)
val choice : ('s, 'a) parser list -> ('s, 'a) parser

(** [seq l] is a combinator that turns a list of parser [l] into a single one
   which will match all of them and return the result in a list. *)
val seq : ('s, 'a) parser list -> ('s, 'a list) parser

(** [between open p close] parses the parser [open], then [p] and [close] and
   returns the value of p. *)
val between : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'c) parser -> ('s, 'b) parser

(** [sepBy sep p] is a parser that parses 0 or more times the parser [p]
   separated by the parser [sep]. *)
val sepBy : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b list) parser

(** [sepBy1 sep p] is a parser that parses 1 or more times the parser [p]
   separated by the parser [sep]. *)
val sepBy1 : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b list) parser

(** [endBy sep p] is a parser that parses 0 or more times the parser [p]
   separated and ended by the parser [sep]. *)
val endBy : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b list) parser

(** [endBy1 sep p] is a parser that parses 1 or more times the parser [p]
   separated and ended by the parser [sep]. *)
val endBy1 : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b list) parser

(** [sepEndBy sep p] is a parser that parses 0 or more times the parser [p]
   separated and optionally ended by the parser [sep]. *)
val sepEndBy : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b list) parser

(** [sepEndBy1 sep p] is a parser that parses 1 or more times the parser [p]
   separated and optionally ended by the parser [sep]. *)
val sepEndBy1 : ('s, 'a) parser -> ('s, 'b) parser -> ('s, 'b list) parser

(* Lazy combinators ************************************************************)

(** [p <*>| q] is the lazy sequence combinator appliying the result of parser
   [p] to the parser [q], but only evaluating the parser [q] if [p] worked. *)
val ( <*>| ) : ('s, 'b -> 'a) parser -> ('s, 'b) parser lazy_t -> ('s, 'a) parser

(** [p <|>| q] is the lazy choice combinator trying the parser [p], if it works,
   returns the result, else evaluate the parser [q] and returns it result. *)
val ( <|>| ) : ('s, 'a) parser -> ('s, 'a) parser lazy_t -> ('s, 'a) parser

(** [p *>| q] is the lazy sequence combinator but ignores value returned by the
   parser [p], it's the missing bracket. The parser [q] is evaluated only if [p]
   succeeded. *)
val ( *>| ) : ('s, 'a) parser -> ('s, 'b) parser lazy_t -> ('s, 'b) parser

(** [p <*| q] is the sequence combinator but ignores value returned by the parser
   [q], it's the missing bracket. The parser [q] is evaluated only if [p]
   succeeded. *)
val ( <*| ) : ('s, 'a) parser -> ('s, 'b) parser lazy_t -> ('s, 'a) parser

(* Basic parsers ***************************************************************)

(** [satisfy p is a parser that matches an element satisfying the predicate
   [p]. *)
val satisfy : ('a -> bool) -> ('a, 'a) parser

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

(** [chainl1 op p] is a parser that parses the operand [p], as left-associative,
   separated by the separator [op], one or more times. *)
val chainl1 : ('s, 'a -> 'a -> 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser

(** [chainl op p default] is a parser that parses the operand [p], as
   left-associative, separated by the separator [op], if it failed, returns the
   value [default]. *)
val chainl : ('s, 'a -> 'a -> 'a) parser -> ('s, 'a) parser -> 'a -> ('s, 'a) parser

(** [chainr1 op p] is a parser that parses the operand [p], as right-associative,
   separated by the separator [op], one or more times. *)
val chainr1 : ('s, 'a -> 'a -> 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser

(** [chainr op p default] is a parser that parses the operand [p], as
   right-associative, separated by the separator [op], if it failed, returns the
   value [default]. *)
val chainr : ('s, 'a -> 'a -> 'a) parser -> ('s, 'a) parser -> 'a -> ('s, 'a) parser

(** [sym s] is a parser that matches the symbol [s]. *)
val sym : 'a -> ('a, 'a) parser

(** [syms s] is a parser that matches the list of symbol [s]. *)
val syms : 'a list -> ('a, 'a list) parser

(** [char c] is a parser that matches the character [c]. *)
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

(** [letter] is a parser that matches an alphabet character. *)
val letter : (char, char) parser

(** [digit] is a parser that matches a digit. *)
val digit : (char, char) parser

(** [alphaNum] is a parser that matches a letter or a digit. *)
val alphaNum : (char, char) parser

(** [octDigit] is a parser that matches an octal digit. *)
val octDigit : (char, char) parser

(** [hexDigit] is a parser that matches a hexadecimal digit. *)
val hexDigit : (char, char) parser

(** [space] is a parser that matches a space. *)
val space : (char, char) parser

(** [spaces] is a parser that matches 0 or more spaces. *)
val spaces : (char, char list) parser

(** [newline] is a parser that matches a newline character. *)
val newline : (char, char) parser

(** [tab] is a parser that matches a tab character. *)
val tab : (char, char) parser

(** [pack l p r] is a parser that matches the parser [p] between the symbols [l]
   and [r]. *)
val pack : 's list -> ('s, 'a) parser -> 's list -> ('s, 'a) parser

(** [packs l p r] is a parser that matches the parser [p] between the strings
   [l] and [r]. *)
val packs : string -> (char, 'a) parser -> string -> (char, 'a) parser

(** [oneOf l] is a parser that matches a symbol from the list [l]. *)
val oneOf : 'a list -> ('a, 'a) parser

(* Quousque tandem abutere, Catilina, patientia nostra? ************************)
