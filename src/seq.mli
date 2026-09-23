(** Effect-polymorphic Seq module *)

type ('a, 'e) eff = unit -['e]-> ('a, 'e) node
and ('a, 'e) node =
  | Nil
  | Cons of 'a * ('a, 'e) eff

type 'a t = ('a, -[]-) eff

val empty : 'a t
val return : 'a -> 'a t
val cons : 'a -> ('a, 'e) eff -> ('a, 'e) eff

val of_dispenser : (unit -['e]-> 'a option) -> ('a, 'e) eff
val to_dispenser : ('a, 'e) eff -> (unit -['e]-> 'a option)

val of_list : 'a list -> 'a t
val to_list : ('a, 'e) eff -['e]-> 'a list

val take : int -> ('a, 'e) eff -> ('a, 'e) eff
val drop : int -> ('a, 'e) eff -> ('a, 'e) eff

val iter : ('a -['e]-> unit) -> 'a t -['e]-> unit
val iter_eff : ('a -['e]-> unit) -> ('a, 'e) eff -['e]-> unit

val iteri : (int -> 'a -['e]-> unit) -> 'a t -['e]-> unit
val iteri_eff : (int -> 'a -['e]-> unit) -> ('a, 'e) eff -['e]-> unit

val fold_left : ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc
val fold_left_eff : ('acc -> 'a -['e]-> 'acc) -> 'acc -> ('a, 'e) eff -['e]-> 'acc

val for_all : ('a -['e]-> bool) -> 'a t -['e]-> bool
val exists : ('a -['e]-> bool) -> 'a t -['e]-> bool

val find : ('a -['e]-> bool) -> 'a t -['e]-> 'a
val find_opt : ('a -['e]-> bool) -> 'a t -['e]-> 'a option
val find_index : ('a -['e]-> bool) -> 'a t -['e]-> int option
val find_map : ('a -['e]-> 'b option) -> 'a t -['e]-> 'b option

val filter : ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff
val filter_map : ('a -['e]-> 'b option) -> ('a, 'e) eff -> ('b, 'e) eff
val map : ('a -['e]-> 'b) -> ('a, 'e) eff -> ('b, 'e) eff

val append : ('a, 'e) eff -> ('a, 'e) eff -> ('a, 'e) eff
val concat : (('a, 'e) eff, 'e) eff -> ('a, 'e) eff
val flat_map : ('a -['e]-> ('b, 'e) eff) -> ('a, 'e) eff -> ('b, 'e) eff
val concat_map : ('a -['e]-> ('b, 'e) eff) -> ('a, 'e) eff -> ('b, 'e) eff

val memoize : 'a t -> 'a t
val once : ('a, 'e) eff -> ('a, 'e) eff

val to_stdlib : 'a t -> 'a Stdlib.Seq.t
val of_stdlib : 'a Stdlib.Seq.t -> 'a t
