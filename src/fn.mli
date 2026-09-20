(** Effect-polymorphic Fun module *)

include module type of Stdlib.Fun

val compose : ('b -['e]-> 'c) -> ('a -['e]-> 'b) -> 'a -['e]-> 'c

val flip : ('a -> 'b -['e]-> 'c) -> ('b -> 'a -['e]-> 'c)

val negate : ('a -['e]-> bool) -> ('a -['e]-> bool)

val protect : finally:(unit -['e]-> unit) -> (unit -['e]-> 'a) -['e]-> 'a
