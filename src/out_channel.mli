(** Effect-polymorphic Out_channel module *)

include module type of Stdlib.Out_channel
  with type t = out_channel
   and type open_flag = Stdlib.open_flag

val with_open_bin : string -> (t -['e]-> 'a) -['e]-> 'a
val with_open_text : string -> (t -['e]-> 'a) -['e]-> 'a
val with_open_gen : open_flag list -> int -> string -> (t -['e]-> 'a) -['e]-> 'a
