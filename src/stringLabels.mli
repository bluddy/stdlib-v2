(** Effect-polymorphic StringLabels module *)

include module type of Stdlib.StringLabels with type t = string

val init : int -> f:(int -['e]-> char) -['e]-> string
val iter : f:(char -['e]-> unit) -> string -['e]-> unit
val iteri : f:(int -> char -['e]-> unit) -> string -['e]-> unit
val map : f:(char -['e]-> char) -> string -['e]-> string
val mapi : f:(int -> char -['e]-> char) -> string -['e]-> string
val fold_left : f:('acc -> char -['e]-> 'acc) -> init:'acc -> string -['e]-> 'acc
val fold_right : f:(char -> 'acc -['e]-> 'acc) -> string -> init:'acc -['e]-> 'acc
val for_all : f:(char -['e]-> bool) -> string -['e]-> bool
val exists : f:(char -['e]-> bool) -> string -['e]-> bool

val take_first_while : (char -['e]-> bool) -> string -['e]-> string
val take_last_while : (char -['e]-> bool) -> string -['e]-> string
val drop_first_while : (char -['e]-> bool) -> string -['e]-> string
val drop_last_while : (char -['e]-> bool) -> string -['e]-> string
val cut_first_while : (char -['e]-> bool) -> string -['e]-> string * string
val cut_last_while : (char -['e]-> bool) -> string -['e]-> string * string

val find_first_index : (char -['e]-> bool) -> ?start:int -> string -['e]-> int option
val find_last_index : (char -['e]-> bool) -> ?start:int -> string -['e]-> int option

val to_seq : string -> (char, 'e) Seq.eff
val to_seqi : string -> (int * char, 'e) Seq.eff
val of_seq : (char, 'e) Seq.eff -['e]-> string
