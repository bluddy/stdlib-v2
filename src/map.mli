(** Effect-polymorphic Map module *)

module type OrderedType = Stdlib.Map.OrderedType

module type S = sig
  type key
  type !+'a t

  val empty: 'a t
  val is_empty: 'a t -> bool
  val is_singleton: 'a t -> bool
  val singleton_to_binding: 'a t -> (key * 'a) option
  val mem: key -> 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val add_to_list: key -> 'a -> 'a list t -> 'a list t
  val update: key -> ('a option -['e]-> 'a option) -> 'a t -['e]-> 'a t
  val singleton: key -> 'a -> 'a t
  val remove: key -> 'a t -> 'a t
  val merge:
    (key -> 'a option -> 'b option -['e]-> 'c option) ->
    'a t -> 'b t -['e]-> 'c t
  val union: (key -> 'a -> 'a -['e]-> 'a option) -> 'a t -> 'a t -['e]-> 'a t
  val cardinal: 'a t -> int
  val bindings: 'a t -> (key * 'a) list
  val min_binding: 'a t -> (key * 'a)
  val min_binding_opt: 'a t -> (key * 'a) option
  val max_binding: 'a t -> (key * 'a)
  val max_binding_opt: 'a t -> (key * 'a) option
  val choose: 'a t -> (key * 'a)
  val choose_opt: 'a t -> (key * 'a) option
  val find: key -> 'a t -> 'a
  val find_opt: key -> 'a t -> 'a option
  val find_first: (key -['e]-> bool) -> 'a t -['e]-> key * 'a
  val find_first_opt: (key -['e]-> bool) -> 'a t -['e]-> (key * 'a) option
  val find_last: (key -['e]-> bool) -> 'a t -['e]-> key * 'a
  val find_last_opt: (key -['e]-> bool) -> 'a t -['e]-> (key * 'a) option
  val iter: (key -> 'a -['e]-> unit) -> 'a t -['e]-> unit
  val fold: (key -> 'a -> 'acc -['e]-> 'acc) -> 'a t -> 'acc -['e]-> 'acc
  val map: ('a -['e]-> 'b) -> 'a t -['e]-> 'b t
  val mapi: (key -> 'a -['e]-> 'b) -> 'a t -['e]-> 'b t
  val filter: (key -> 'a -['e]-> bool) -> 'a t -['e]-> 'a t
  val filter_map: (key -> 'a -['e]-> 'b option) -> 'a t -['e]-> 'b t
  val partition: (key -> 'a -['e]-> bool) -> 'a t -['e]-> 'a t * 'a t
  val split: key -> 'a t -> 'a t * 'a option * 'a t
  val equal: ('a -> 'a -['e]-> bool) -> 'a t -> 'a t -['e]-> bool
  val compare: ('a -> 'a -['e]-> int) -> 'a t -> 'a t -['e]-> int
  val for_all: (key -> 'a -['e]-> bool) -> 'a t -['e]-> bool
  val exists: (key -> 'a -['e]-> bool) -> 'a t -['e]-> bool
  val to_list: 'a t -> (key * 'a) list
  val of_list: (key * 'a) list -> 'a t

  val to_seq: 'a t -> (key * 'a, 'e) Seq.eff
  val to_rev_seq: 'a t -> (key * 'a, 'e) Seq.eff
  val to_seq_from: key -> 'a t -> (key * 'a, 'e) Seq.eff
  val add_seq: (key * 'a, 'e) Seq.eff -> 'a t -['e]-> 'a t
  val of_seq: (key * 'a, 'e) Seq.eff -['e]-> 'a t
end

module Make (Ord : OrderedType) : S with type key = Ord.t and type !+'a t = 'a Stdlib.Map.Make(Ord).t
