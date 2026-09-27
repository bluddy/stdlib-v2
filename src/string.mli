(** Effect-polymorphic String module *)

include module type of Stdlib.String with type t = string

val init : int -> (int -['e]-> char) -['e]-> string
val iter : (char -['e]-> unit) -> string -['e]-> unit
val iteri : (int -> char -['e]-> unit) -> string -['e]-> unit
val map : (char -['e]-> char) -> string -['e]-> string
val mapi : (int -> char -['e]-> char) -> string -['e]-> string
val fold_left : ('acc -> char -['e]-> 'acc) -> 'acc -> string -['e]-> 'acc
val fold_right : (char -> 'acc -['e]-> 'acc) -> string -> 'acc -['e]-> 'acc
val for_all : (char -['e]-> bool) -> string -['e]-> bool
val exists : (char -['e]-> bool) -> string -['e]-> bool

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
