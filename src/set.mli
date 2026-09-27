(** Effect-polymorphic Set module *)

module type OrderedType = Stdlib.Set.OrderedType

module type S = sig
  type elt
  type t

  val empty: t
  val is_empty: t -> bool
  val is_singleton: t -> bool
  val singleton_to_elt: t -> elt option
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val disjoint: t -> t -> bool
  val diff: t -> t -> t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt: t -> elt
  val min_elt_opt: t -> elt option
  val max_elt: t -> elt
  val max_elt_opt: t -> elt option
  val choose: t -> elt
  val choose_opt: t -> elt option
  val find: elt -> t -> elt
  val find_opt: elt -> t -> elt option
  val find_first: (elt -['e]-> bool) -> t -['e]-> elt
  val find_first_opt: (elt -['e]-> bool) -> t -['e]-> elt option
  val find_last: (elt -['e]-> bool) -> t -['e]-> elt
  val find_last_opt: (elt -['e]-> bool) -> t -['e]-> elt option
  val iter: (elt -['e]-> unit) -> t -['e]-> unit
  val fold: (elt -> 'acc -['e]-> 'acc) -> t -> 'acc -['e]-> 'acc
  val map: (elt -['e]-> elt) -> t -['e]-> t
  val filter: (elt -['e]-> bool) -> t -['e]-> t
  val filter_map: (elt -['e]-> elt option) -> t -['e]-> t
  val partition: (elt -['e]-> bool) -> t -['e]-> t * t
  val split: elt -> t -> t * bool * t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val subset: t -> t -> bool
  val for_all: (elt -['e]-> bool) -> t -['e]-> bool
  val exists: (elt -['e]-> bool) -> t -['e]-> bool
  val to_list: t -> elt list
  val of_list: elt list -> t

  val to_seq: t -> (elt, 'e) Seq.eff
  val to_rev_seq: t -> (elt, 'e) Seq.eff
  val to_seq_from: elt -> t -> (elt, 'e) Seq.eff
  val add_seq: (elt, 'e) Seq.eff -> t -['e]-> t
  val of_seq: (elt, 'e) Seq.eff -['e]-> t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t and type t = Stdlib.Set.Make(Ord).t
