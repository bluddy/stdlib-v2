(** Effect-polymorphic ListLabels module *)

include module type of Stdlib.ListLabels with type 'a t = 'a list

val init : len:int -> f:(int -['e]-> 'a) -['e]-> 'a list

val iter : f:('a -['e]-> unit) -> 'a list -['e]-> unit
val iteri : f:(int -> 'a -['e]-> unit) -> 'a list -['e]-> unit

val map : f:('a -['e]-> 'b) -> 'a list -['e]-> 'b list
val mapi : f:(int -> 'a -['e]-> 'b) -> 'a list -['e]-> 'b list
val rev_map : f:('a -['e]-> 'b) -> 'a list -['e]-> 'b list

val filter_map : f:('a -['e]-> 'b option) -> 'a list -['e]-> 'b list
val filter_mapi : f:(int -> 'a -['e]-> 'b option) -> 'a list -['e]-> 'b list
val concat_map : f:('a -['e]-> 'b list) -> 'a list -['e]-> 'b list

val fold_left_map :
  f:('acc -> 'a -['e]-> 'acc * 'b) -> init:'acc -> 'a list -['e]-> 'acc * 'b list

val fold_left : f:('acc -> 'a -['e]-> 'acc) -> init:'acc -> 'a list -['e]-> 'acc
val fold_right : f:('a -> 'acc -['e]-> 'acc) -> 'a list -> init:'acc -['e]-> 'acc

val iter2 : f:('a -> 'b -['e]-> unit) -> 'a list -> 'b list -['e]-> unit
val map2 : f:('a -> 'b -['e]-> 'c) -> 'a list -> 'b list -['e]-> 'c list
val rev_map2 : f:('a -> 'b -['e]-> 'c) -> 'a list -> 'b list -['e]-> 'c list
val fold_left2 :
  f:('acc -> 'a -> 'b -['e]-> 'acc) -> init:'acc -> 'a list -> 'b list -['e]-> 'acc
val fold_right2 :
  f:('a -> 'b -> 'acc -['e]-> 'acc) -> 'a list -> 'b list -> init:'acc -['e]-> 'acc

val for_all : f:('a -['e]-> bool) -> 'a list -['e]-> bool
val exists : f:('a -['e]-> bool) -> 'a list -['e]-> bool
val for_all2 : f:('a -> 'b -['e]-> bool) -> 'a list -> 'b list -['e]-> bool
val exists2 : f:('a -> 'b -['e]-> bool) -> 'a list -> 'b list -['e]-> bool

val find : f:('a -['e]-> bool) -> 'a list -['e]-> 'a
val find_opt : f:('a -['e]-> bool) -> 'a list -['e]-> 'a option
val find_index : f:('a -['e]-> bool) -> 'a list -['e]-> int option
val find_map : f:('a -['e]-> 'b option) -> 'a list -['e]-> 'b option
val find_mapi : f:(int -> 'a -['e]-> 'b option) -> 'a list -['e]-> 'b option

val filter : f:('a -['e]-> bool) -> 'a list -['e]-> 'a list
val find_all : f:('a -['e]-> bool) -> 'a list -['e]-> 'a list
val filteri : f:(int -> 'a -['e]-> bool) -> 'a list -['e]-> 'a list

val take_while : f:('a -['e]-> bool) -> 'a list -['e]-> 'a list
val drop_while : f:('a -['e]-> bool) -> 'a list -['e]-> 'a list

val partition : f:('a -['e]-> bool) -> 'a list -['e]-> 'a list * 'a list
val partition_map :
  f:('a -['e]-> ('b, 'c) Either.t) -> 'a list -['e]-> 'b list * 'c list

val sort : cmp:('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val stable_sort : cmp:('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val fast_sort : cmp:('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val sort_uniq : cmp:('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val merge : cmp:('a -> 'a -['e]-> int) -> 'a list -> 'a list -['e]-> 'a list

val to_seq : 'a list -> ('a, 'e) Seq.eff
val of_seq : ('a, 'e) Seq.eff -['e]-> 'a list
