(** Effect-polymorphic Bytes module *)

include module type of Stdlib.Bytes with type t = bytes

val init : int -> (int -['e]-> char) -['e]-> bytes
val iter : (char -['e]-> unit) -> bytes -['e]-> unit
val iteri : (int -> char -['e]-> unit) -> bytes -['e]-> unit
val map : (char -['e]-> char) -> bytes -['e]-> bytes
val mapi : (int -> char -['e]-> char) -> bytes -['e]-> bytes
val fold_left : ('acc -> char -['e]-> 'acc) -> 'acc -> bytes -['e]-> 'acc
val fold_right : (char -> 'acc -['e]-> 'acc) -> bytes -> 'acc -['e]-> 'acc
val for_all : (char -['e]-> bool) -> bytes -['e]-> bool
val exists : (char -['e]-> bool) -> bytes -['e]-> bool

val to_seq : bytes -> (char, 'e) Seq.eff
val to_seqi : bytes -> (int * char, 'e) Seq.eff
val of_seq : (char, 'e) Seq.eff -['e]-> bytes
