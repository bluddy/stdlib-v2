(** Effect-polymorphic Seq module *)

type ('a, 'e) eff = unit -['e]-> ('a, 'e) node
and ('a, 'e) node =
  | Nil
  | Cons of 'a * ('a, 'e) eff

type 'a t = ('a, -[]-) eff

val empty : ('a, 'e) eff
val return : 'a -> ('a, 'e) eff
val singleton : 'a -> ('a, 'e) eff
val cons : 'a -> ('a, 'e) eff -> ('a, 'e) eff

val of_dispenser : (unit -['e]-> 'a option) -> ('a, 'e) eff
val to_dispenser : ('a, 'e) eff -> (unit -['e]-> 'a option)

val of_list : 'a list -> ('a, 'e) eff
val to_list : ('a, 'e) eff -['e]-> 'a list

val of_pure_seq : 'a t -> ('a, 'e) eff
val of_stdlib_seq : 'a Stdlib.Seq.t -> ('a, 'e) eff

val init : int -> (int -['e]-> 'a) -> ('a, 'e) eff
val unfold : ('b -['e]-> ('a * 'b) option) -> 'b -> ('a, 'e) eff
val iterate : ('a -['e]-> 'a) -> 'a -> ('a, 'e) eff
val forever : (unit -['e]-> 'a) -> ('a, 'e) eff

val take : int -> ('a, 'e) eff -> ('a, 'e) eff
val drop : int -> ('a, 'e) eff -> ('a, 'e) eff
val take_while : ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff
val drop_while : ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff

val iter : ('a -['e]-> unit) -> 'a t -['e]-> unit
val iter_eff : ('a -['e]-> unit) -> ('a, 'e) eff -['e]-> unit

val iteri : (int -> 'a -['e]-> unit) -> 'a t -['e]-> unit
val iteri_eff : (int -> 'a -['e]-> unit) -> ('a, 'e) eff -['e]-> unit

val iter2 : ('a -> 'b -['e]-> unit) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> unit

val fold_left : ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc
val fold_left_eff : ('acc -> 'a -['e]-> 'acc) -> 'acc -> ('a, 'e) eff -['e]-> 'acc

val fold_lefti : ('acc -> int -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc
val fold_lefti_eff : ('acc -> int -> 'a -['e]-> 'acc) -> 'acc -> ('a, 'e) eff -['e]-> 'acc

val fold_left2 : ('acc -> 'a -> 'b -['e]-> 'acc) -> 'acc -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> 'acc

val for_all : ('a -['e]-> bool) -> 'a t -['e]-> bool
val for_all_eff : ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> bool

val exists : ('a -['e]-> bool) -> 'a t -['e]-> bool
val exists_eff : ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> bool

val for_all2 : ('a -> 'b -['e]-> bool) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> bool
val exists2 : ('a -> 'b -['e]-> bool) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> bool

val equal : ('a -> 'b -['e]-> bool) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> bool
val compare : ('a -> 'b -['e]-> int) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> int

val find : ('a -['e]-> bool) -> 'a t -['e]-> 'a
val find_eff : ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> 'a

val find_opt : ('a -['e]-> bool) -> 'a t -['e]-> 'a option
val find_opt_eff : ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> 'a option

val find_index : ('a -['e]-> bool) -> 'a t -['e]-> int option
val find_index_eff : ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> int option

val find_map : ('a -['e]-> 'b option) -> 'a t -['e]-> 'b option
val find_map_eff : ('a -['e]-> 'b option) -> ('a, 'e) eff -['e]-> 'b option

val find_mapi : (int -> 'a -['e]-> 'b option) -> 'a t -['e]-> 'b option
val find_mapi_eff : (int -> 'a -['e]-> 'b option) -> ('a, 'e) eff -['e]-> 'b option

val filter : ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff
val filteri : (int -> 'a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff
val filter_map : ('a -['e]-> 'b option) -> ('a, 'e) eff -> ('b, 'e) eff
val map : ('a -['e]-> 'b) -> ('a, 'e) eff -> ('b, 'e) eff
val mapi : (int -> 'a -['e]-> 'b) -> ('a, 'e) eff -> ('b, 'e) eff
val map2 : ('a -> 'b -['e]-> 'c) -> ('a, 'e) eff -> ('b, 'e) eff -> ('c, 'e) eff

val scan : ('b -> 'a -['e]-> 'b) -> 'b -> ('a, 'e) eff -> ('b, 'e) eff
val zip : ('a, 'e) eff -> ('b, 'e) eff -> ('a * 'b, 'e) eff

val partition : ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff * ('a, 'e) eff
val partition_map : ('a -['e]-> ('b, 'c) Either.t) -> ('a, 'e) eff -> ('b, 'e) eff * ('c, 'e) eff

val append : ('a, 'e) eff -> ('a, 'e) eff -> ('a, 'e) eff
val concat : (('a, 'e) eff, 'e) eff -> ('a, 'e) eff
val flat_map : ('a -['e]-> ('b, 'e) eff) -> ('a, 'e) eff -> ('b, 'e) eff
val concat_map : ('a -['e]-> ('b, 'e) eff) -> ('a, 'e) eff -> ('b, 'e) eff

val memoize : 'a t -> 'a t
val once : ('a, 'e) eff -> ('a, 'e) eff

val to_stdlib : 'a t -> 'a Stdlib.Seq.t
val of_stdlib : 'a Stdlib.Seq.t -> 'a t
