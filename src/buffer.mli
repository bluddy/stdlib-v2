(** Effect-polymorphic Buffer module *)

include module type of Stdlib.Buffer with type t = Stdlib.Buffer.t

val add_substitute : t -> (string -['e]-> string) -> string -['e]-> unit

val to_seq : t -> (char, 'e) Seq.eff
val to_seqi : t -> (int * char, 'e) Seq.eff
val add_seq : t -> (char, 'e) Seq.eff -['e]-> unit
val of_seq : (char, 'e) Seq.eff -['e]-> t
