type ('a, 'b) parser = 'a list -> ('b * 'a list) option

val return : 'a -> ('s, 'a) parser
val fail : ('s, 'a) parser
val (<*>) : ('s, 'b -> 'a) parser -> ('s, 'b) parser -> ('s, 'a) parser
val (<|>) : ('s, 'a) parser -> ('s, 'a) parser -> ('s, 'a) parser
