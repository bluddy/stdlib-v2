(** Effect-polymorphic Stack module *)

include module type of Stdlib.Stack with type 'a t = 'a Stdlib.Stack.t

val iter : ('a -['e]-> unit) -> 'a t -['e]-> unit
val fold : ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc

val to_seq : 'a t -> ('a, 'e) Seq.eff
val add_seq : 'a t -> ('a, 'e) Seq.eff -['e]-> unit
val of_seq : ('a, 'e) Seq.eff -['e]-> 'a t
