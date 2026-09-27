(** Effect-polymorphic ArrayLabels module *)

include module type of Stdlib.ArrayLabels with type 'a t = 'a array

val init : int -> f:(int -['e]-> 'a) -['e]-> 'a array

val iter : f:('a -['e]-> unit) -> 'a array -['e]-> unit
val iteri : f:(int -> 'a -['e]-> unit) -> 'a array -['e]-> unit

val map : f:('a -['e]-> 'b) -> 'a array -['e]-> 'b array
val mapi : f:(int -> 'a -['e]-> 'b) -> 'a array -['e]-> 'b array

val fold_left : f:('acc -> 'a -['e]-> 'acc) -> init:'acc -> 'a array -['e]-> 'acc
val fold_right : f:('a -> 'acc -['e]-> 'acc) -> 'a array -> init:'acc -['e]-> 'acc
val fold_lefti : f:('acc -> int -> 'a -['e]-> 'acc) -> init:'acc -> 'a array -['e]-> 'acc
val fold_righti : f:(int -> 'a -> 'acc -['e]-> 'acc) -> 'a array -> init:'acc -['e]-> 'acc
val fold_left_map :
  f:('acc -> 'a -['e]-> 'acc * 'b) -> init:'acc -> 'a array -['e]-> 'acc * 'b array

val filter : f:('a -['e]-> bool) -> 'a array -['e]-> 'a array
val filter_map : f:('a -['e]-> 'b option) -> 'a array -['e]-> 'b array
val concat_map : f:('a -['e]-> 'b array) -> 'a array -['e]-> 'b array

val iter2 : f:('a -> 'b -['e]-> unit) -> 'a array -> 'b array -['e]-> unit
val map2 : f:('a -> 'b -['e]-> 'c) -> 'a array -> 'b array -['e]-> 'c array

val for_all : f:('a -['e]-> bool) -> 'a array -['e]-> bool
val exists : f:('a -['e]-> bool) -> 'a array -['e]-> bool
val for_all2 : f:('a -> 'b -['e]-> bool) -> 'a array -> 'b array -['e]-> bool
val exists2 : f:('a -> 'b -['e]-> bool) -> 'a array -> 'b array -['e]-> bool

val find_opt : f:('a -['e]-> bool) -> 'a array -['e]-> 'a option
val find_index : f:('a -['e]-> bool) -> 'a array -['e]-> int option
val find_map : f:('a -['e]-> 'b option) -> 'a array -['e]-> 'b option
val find_mapi : f:(int -> 'a -['e]-> 'b option) -> 'a array -['e]-> 'b option

val sort : cmp:('a -> 'a -['e]-> int) -> 'a array -['e]-> unit
val stable_sort : cmp:('a -> 'a -['e]-> int) -> 'a array -['e]-> unit
val fast_sort : cmp:('a -> 'a -['e]-> int) -> 'a array -['e]-> unit

val to_seq : 'a array -> ('a, 'e) Seq.eff
val to_seqi : 'a array -> (int * 'a, 'e) Seq.eff
val of_seq : ('a, 'e) Seq.eff -['e]-> 'a array
