(** Effect-polymorphic Queue module *)

include module type of Stdlib.Queue with type 'a t = 'a Stdlib.Queue.t

val iter : ('a -['e]-> unit) -> 'a t -['e]-> unit
val fold : ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc

val to_seq : 'a t -> ('a, 'e) Seq.eff
val add_seq : 'a t -> ('a, 'e) Seq.eff -['e]-> unit
val of_seq : ('a, 'e) Seq.eff -['e]-> 'a t
