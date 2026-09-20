(** Effect-polymorphic Option module *)

include module type of Stdlib.Option

val map : ('a -['e]-> 'b) -> 'a option -['e]-> 'b option

val bind : 'a option -> ('a -['e]-> 'b option) -['e]-> 'b option

val iter : ('a -['e]-> unit) -> 'a option -['e]-> unit

val fold : none:'acc -> some:('a -['e]-> 'acc) -> 'a option -['e]-> 'acc

val for_all : ('a -['e]-> bool) -> 'a option -['e]-> bool

val exists : ('a -['e]-> bool) -> 'a option -['e]-> bool

val filter : ('a -['e]-> bool) -> 'a option -['e]-> 'a option
