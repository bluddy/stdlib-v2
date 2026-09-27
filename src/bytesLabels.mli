(** Effect-polymorphic BytesLabels module *)

include module type of Stdlib.BytesLabels with type t = bytes

val init : int -> f:(int -['e]-> char) -['e]-> bytes
val iter : f:(char -['e]-> unit) -> bytes -['e]-> unit
val iteri : f:(int -> char -['e]-> unit) -> bytes -['e]-> unit
val map : f:(char -['e]-> char) -> bytes -['e]-> bytes
val mapi : f:(int -> char -['e]-> char) -> bytes -['e]-> bytes
val fold_left : f:('acc -> char -['e]-> 'acc) -> init:'acc -> bytes -['e]-> 'acc
val fold_right : f:(char -> 'acc -['e]-> 'acc) -> bytes -> init:'acc -['e]-> 'acc
val for_all : f:(char -['e]-> bool) -> bytes -['e]-> bool
val exists : f:(char -['e]-> bool) -> bytes -['e]-> bool

val to_seq : bytes -> (char, 'e) Seq.eff
val to_seqi : bytes -> (int * char, 'e) Seq.eff
val of_seq : (char, 'e) Seq.eff -['e]-> bytes
