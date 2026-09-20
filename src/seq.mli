(** Effect-polymorphic Seq module *)

include module type of Stdlib.Seq with type 'a t = 'a Stdlib.Seq.t and type 'a node = 'a Stdlib.Seq.node

val iter : ('a -['e]-> unit) -> 'a t -['e]-> unit
val iteri : (int -> 'a -['e]-> unit) -> 'a t -['e]-> unit

val fold_left : ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc

val for_all : ('a -['e]-> bool) -> 'a t -['e]-> bool
val exists : ('a -['e]-> bool) -> 'a t -['e]-> bool

val find : ('a -['e]-> bool) -> 'a t -['e]-> 'a
val find_opt : ('a -['e]-> bool) -> 'a t -['e]-> 'a option
val find_index : ('a -['e]-> bool) -> 'a t -['e]-> int option
val find_map : ('a -['e]-> 'b option) -> 'a t -['e]-> 'b option
