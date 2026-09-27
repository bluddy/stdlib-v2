(** Effect-polymorphic Array module *)

include module type of Stdlib.Array with type 'a t = 'a array

val init : int -> (int -['e]-> 'a) -['e]-> 'a array

val iter : ('a -['e]-> unit) -> 'a array -['e]-> unit
val iteri : (int -> 'a -['e]-> unit) -> 'a array -['e]-> unit

val map : ('a -['e]-> 'b) -> 'a array -['e]-> 'b array
val mapi : (int -> 'a -['e]-> 'b) -> 'a array -['e]-> 'b array

val fold_left : ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a array -['e]-> 'acc
val fold_right : ('a -> 'acc -['e]-> 'acc) -> 'a array -> 'acc -['e]-> 'acc
val fold_left_map :
  ('acc -> 'a -['e]-> 'acc * 'b) -> 'acc -> 'a array -['e]-> 'acc * 'b array

val iter2 : ('a -> 'b -['e]-> unit) -> 'a array -> 'b array -['e]-> unit
val map2 : ('a -> 'b -['e]-> 'c) -> 'a array -> 'b array -['e]-> 'c array

val for_all : ('a -['e]-> bool) -> 'a array -['e]-> bool
val exists : ('a -['e]-> bool) -> 'a array -['e]-> bool
val for_all2 : ('a -> 'b -['e]-> bool) -> 'a array -> 'b array -['e]-> bool
val exists2 : ('a -> 'b -['e]-> bool) -> 'a array -> 'b array -['e]-> bool

val find_opt : ('a -['e]-> bool) -> 'a array -['e]-> 'a option
val find_index : ('a -['e]-> bool) -> 'a array -['e]-> int option
val find_map : ('a -['e]-> 'b option) -> 'a array -['e]-> 'b option
val find_mapi : (int -> 'a -['e]-> 'b option) -> 'a array -['e]-> 'b option

val sort : ('a -> 'a -['e]-> int) -> 'a array -['e]-> unit
val stable_sort : ('a -> 'a -['e]-> int) -> 'a array -['e]-> unit
val fast_sort : ('a -> 'a -['e]-> int) -> 'a array -['e]-> unit

val to_seq : 'a array -> ('a, 'e) Seq.eff
val to_seqi : 'a array -> (int * 'a, 'e) Seq.eff
val of_seq : ('a, 'e) Seq.eff -['e]-> 'a array
