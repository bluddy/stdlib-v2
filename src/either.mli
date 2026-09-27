(** Effect-polymorphic Either module *)

include module type of Stdlib.Either

val map_left : ('a1 -['e]-> 'a2) -> ('a1, 'b) t -['e]-> ('a2, 'b) t
val map_right : ('b1 -['e]-> 'b2) -> ('a, 'b1) t -['e]-> ('a, 'b2) t
val map :
  left:('a1 -['e]-> 'a2) -> right:('b1 -['e]-> 'b2) -> ('a1, 'b1) t -['e]-> ('a2, 'b2) t
val fold : left:('a -['e]-> 'c) -> right:('b -['e]-> 'c) -> ('a, 'b) t -['e]-> 'c
val iter : left:('a -['e]-> unit) -> right:('b -['e]-> unit) -> ('a, 'b) t -['e]-> unit
val for_all : left:('a -['e]-> bool) -> right:('b -['e]-> bool) -> ('a, 'b) t -['e]-> bool
val equal :
  left:('a -> 'a -['e]-> bool) -> right:('b -> 'b -['e]-> bool) ->
  ('a, 'b) t -> ('a, 'b) t -['e]-> bool
val compare :
  left:('a -> 'a -['e]-> int) -> right:('b -> 'b -['e]-> int) ->
  ('a, 'b) t -> ('a, 'b) t -['e]-> int
