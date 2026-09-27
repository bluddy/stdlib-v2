(** Effect-polymorphic Hashtbl module *)

module type HashedType = Stdlib.Hashtbl.HashedType
module type SeededHashedType = Stdlib.Hashtbl.SeededHashedType

module type S = sig
  type key
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find_and_remove : 'a t -> key -> 'a option
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val find_and_replace : 'a t -> key -> 'a -> 'a option
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -['e]-> unit) -> 'a t -['e]-> unit
  val filter_map_inplace : (key -> 'a -['e]-> 'a option) -> 'a t -['e]-> unit
  val fold : (key -> 'a -> 'acc -['e]-> 'acc) -> 'a t -> 'acc -['e]-> 'acc
  val length : 'a t -> int
  val stats : 'a t -> Stdlib.Hashtbl.statistics

  val to_seq : 'a t -> (key * 'a, 'e) Seq.eff
  val to_seq_keys : _ t -> (key, 'e) Seq.eff
  val to_seq_values : 'a t -> ('a, 'e) Seq.eff
  val add_seq : 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit
  val replace_seq : 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit
  val of_seq : (key * 'a, 'e) Seq.eff -['e]-> 'a t
end

module type SeededS = sig
  type key
  type !'a t

  val create : ?random:bool -> int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find_and_remove : 'a t -> key -> 'a option
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val find_and_replace : 'a t -> key -> 'a -> 'a option
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -['e]-> unit) -> 'a t -['e]-> unit
  val filter_map_inplace : (key -> 'a -['e]-> 'a option) -> 'a t -['e]-> unit
  val fold : (key -> 'a -> 'acc -['e]-> 'acc) -> 'a t -> 'acc -['e]-> 'acc
  val length : 'a t -> int
  val stats : 'a t -> Stdlib.Hashtbl.statistics

  val to_seq : 'a t -> (key * 'a, 'e) Seq.eff
  val to_seq_keys : _ t -> (key, 'e) Seq.eff
  val to_seq_values : 'a t -> ('a, 'e) Seq.eff
  val add_seq : 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit
  val replace_seq : 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit
  val of_seq : (key * 'a, 'e) Seq.eff -['e]-> 'a t
end

include module type of Stdlib.Hashtbl
  with type ('a, 'b) t = ('a, 'b) Stdlib.Hashtbl.t
   and module type HashedType := Stdlib.Hashtbl.HashedType
   and module type SeededHashedType := Stdlib.Hashtbl.SeededHashedType
   and module type S := Stdlib.Hashtbl.S
   and module type SeededS := Stdlib.Hashtbl.SeededS
   and module Make := Stdlib.Hashtbl.Make
   and module MakeSeeded := Stdlib.Hashtbl.MakeSeeded

val iter : ('a -> 'b -['e]-> unit) -> ('a, 'b) t -['e]-> unit
val filter_map_inplace : ('a -> 'b -['e]-> 'b option) -> ('a, 'b) t -['e]-> unit
val fold : ('a -> 'b -> 'acc -['e]-> 'acc) -> ('a, 'b) t -> 'acc -['e]-> 'acc

val to_seq : ('a, 'b) t -> ('a * 'b, 'e) Seq.eff
val to_seq_keys : ('a, _) t -> ('a, 'e) Seq.eff
val to_seq_values : (_, 'b) t -> ('b, 'e) Seq.eff
val add_seq : ('a, 'b) t -> ('a * 'b, 'e) Seq.eff -['e]-> unit
val replace_seq : ('a, 'b) t -> ('a * 'b, 'e) Seq.eff -['e]-> unit
val of_seq : ('a * 'b, 'e) Seq.eff -['e]-> ('a, 'b) t

module Make (H : HashedType) : S with type key = H.t and type 'a t = 'a Stdlib.Hashtbl.Make(H).t
module MakeSeeded (H : SeededHashedType) : SeededS with type key = H.t and type 'a t = 'a Stdlib.Hashtbl.MakeSeeded(H).t
