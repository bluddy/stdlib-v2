(** Effect-polymorphic List module *)

include module type of Stdlib.List with type 'a t = 'a list

val init : int -> (int -['e]-> 'a) -['e]-> 'a list

val iter : ('a -['e]-> unit) -> 'a list -['e]-> unit
val iteri : (int -> 'a -['e]-> unit) -> 'a list -['e]-> unit

val map : ('a -['e]-> 'b) -> 'a list -['e]-> 'b list
val mapi : (int -> 'a -['e]-> 'b) -> 'a list -['e]-> 'b list
val rev_map : ('a -['e]-> 'b) -> 'a list -['e]-> 'b list

val filter_map : ('a -['e]-> 'b option) -> 'a list -['e]-> 'b list
val filter_mapi : (int -> 'a -['e]-> 'b option) -> 'a list -['e]-> 'b list
val concat_map : ('a -['e]-> 'b list) -> 'a list -['e]-> 'b list

val fold_left_map :
  ('acc -> 'a -['e]-> 'acc * 'b) -> 'acc -> 'a list -['e]-> 'acc * 'b list

val fold_left : ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a list -['e]-> 'acc
val fold_right : ('a -> 'acc -['e]-> 'acc) -> 'a list -> 'acc -['e]-> 'acc

val iter2 : ('a -> 'b -['e]-> unit) -> 'a list -> 'b list -['e]-> unit
val map2 : ('a -> 'b -['e]-> 'c) -> 'a list -> 'b list -['e]-> 'c list
val rev_map2 : ('a -> 'b -['e]-> 'c) -> 'a list -> 'b list -['e]-> 'c list
val fold_left2 :
  ('acc -> 'a -> 'b -['e]-> 'acc) -> 'acc -> 'a list -> 'b list -['e]-> 'acc
val fold_right2 :
  ('a -> 'b -> 'acc -['e]-> 'acc) -> 'a list -> 'b list -> 'acc -['e]-> 'acc

val for_all : ('a -['e]-> bool) -> 'a list -['e]-> bool
val exists : ('a -['e]-> bool) -> 'a list -['e]-> bool
val for_all2 : ('a -> 'b -['e]-> bool) -> 'a list -> 'b list -['e]-> bool
val exists2 : ('a -> 'b -['e]-> bool) -> 'a list -> 'b list -['e]-> bool

val find : ('a -['e]-> bool) -> 'a list -['e]-> 'a
val find_opt : ('a -['e]-> bool) -> 'a list -['e]-> 'a option
val find_index : ('a -['e]-> bool) -> 'a list -['e]-> int option
val find_map : ('a -['e]-> 'b option) -> 'a list -['e]-> 'b option
val find_mapi : (int -> 'a -['e]-> 'b option) -> 'a list -['e]-> 'b option

val filter : ('a -['e]-> bool) -> 'a list -['e]-> 'a list
val find_all : ('a -['e]-> bool) -> 'a list -['e]-> 'a list
val filteri : (int -> 'a -['e]-> bool) -> 'a list -['e]-> 'a list

val take_while : ('a -['e]-> bool) -> 'a list -['e]-> 'a list
val drop_while : ('a -['e]-> bool) -> 'a list -['e]-> 'a list

val partition : ('a -['e]-> bool) -> 'a list -['e]-> 'a list * 'a list
val partition_map :
  ('a -['e]-> ('b, 'c) Either.t) -> 'a list -['e]-> 'b list * 'c list

val sort : ('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val stable_sort : ('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val fast_sort : ('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val sort_uniq : ('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list
val merge : ('a -> 'a -['e]-> int) -> 'a list -> 'a list -['e]-> 'a list

val to_seq : 'a list -> ('a, 'e) Seq.eff
val of_seq : ('a, 'e) Seq.eff -['e]-> 'a list

