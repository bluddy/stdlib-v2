(** Effect-polymorphic In_channel module *)

include module type of Stdlib.In_channel
  with type t = in_channel
   and type open_flag = Stdlib.open_flag

val with_open_bin : string -> (t -['e]-> 'a) -['e]-> 'a
val with_open_text : string -> (t -['e]-> 'a) -['e]-> 'a
val with_open_gen : open_flag list -> int -> string -> (t -['e]-> 'a) -['e]-> 'a
val fold_lines : ('acc -> string -['e]-> 'acc) -> 'acc -> t -['e]-> 'acc
