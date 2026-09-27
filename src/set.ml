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

module Make (Ord : OrderedType) : S with type elt = Ord.t and type t = Stdlib.Set.Make(Ord).t = struct
  module S = Stdlib.Set.Make(Ord)

  type elt = S.elt
  type t = S.t

  let empty = S.empty
  let is_empty = S.is_empty
  let is_singleton = S.is_singleton
  let singleton_to_elt = S.singleton_to_elt
  let mem = S.mem
  let add = S.add
  let singleton = S.singleton
  let remove = S.remove
  let union = S.union
  let inter = S.inter
  let disjoint = S.disjoint
  let diff = S.diff
  let cardinal = S.cardinal
  let elements = S.elements
  let min_elt = S.min_elt
  let min_elt_opt = S.min_elt_opt
  let max_elt = S.max_elt
  let max_elt_opt = S.max_elt_opt
  let choose = S.choose
  let choose_opt = S.choose_opt
  let find = S.find
  let find_opt = S.find_opt
  let split = S.split
  let equal = S.equal
  let compare = S.compare
  let subset = S.subset
  let to_list = S.to_list
  let of_list = S.of_list

  let to_seq s = Seq.of_stdlib_seq (S.to_seq s)
  let to_rev_seq s = Seq.of_stdlib_seq (S.to_rev_seq s)
  let to_seq_from x s = Seq.of_stdlib_seq (S.to_seq_from x s)

  let add_seq : 'e. (elt, 'e) Seq.eff -> t -['e]-> t =
    fun seq s ->
      let f : t -> elt -['e]-> t = fun acc x -> S.add x acc in
      Seq.fold_left_eff f s seq

  let of_seq : 'e. (elt, 'e) Seq.eff -['e]-> t =
    fun seq -> add_seq seq S.empty

  let iter : 'e. (elt -['e]-> unit) -> t -['e]-> unit =
    fun f s -> Seq.iter_eff f (to_seq s)

  let fold : 'acc 'e. (elt -> 'acc -['e]-> 'acc) -> t -> 'acc -['e]-> 'acc =
    fun f s init ->
      let f' : 'acc -> elt -['e]-> 'acc = fun acc x -> f x acc in
      Seq.fold_left_eff f' init (to_seq s)

  let map : 'e. (elt -['e]-> elt) -> t -['e]-> t =
    fun f s ->
      let f' : t -> elt -['e]-> t = fun acc x -> S.add (f x) acc in
      Seq.fold_left_eff f' S.empty (to_seq s)

  let filter : 'e. (elt -['e]-> bool) -> t -['e]-> t =
    fun p s ->
      let f' : t -> elt -['e]-> t = fun acc x -> if p x then S.add x acc else acc in
      Seq.fold_left_eff f' S.empty (to_seq s)

  let filter_map : 'e. (elt -['e]-> elt option) -> t -['e]-> t =
    fun f s ->
      let f' : t -> elt -['e]-> t = fun acc x ->
        match f x with
        | None -> acc
        | Some x' -> S.add x' acc
      in
      Seq.fold_left_eff f' S.empty (to_seq s)

  let partition : 'e. (elt -['e]-> bool) -> t -['e]-> t * t =
    fun p s ->
      let f' : t * t -> elt -['e]-> t * t = fun (s1, s2) x ->
        if p x then (S.add x s1, s2) else (s1, S.add x s2)
      in
      Seq.fold_left_eff f' (S.empty, S.empty) (to_seq s)

  let for_all : 'e. (elt -['e]-> bool) -> t -['e]-> bool =
    fun p s -> Seq.for_all_eff p (to_seq s)

  let exists : 'e. (elt -['e]-> bool) -> t -['e]-> bool =
    fun p s -> Seq.exists_eff p (to_seq s)

  let find_first : 'e. (elt -['e]-> bool) -> t -['e]-> elt =
    fun p s -> Seq.find_eff p (to_seq s)

  let find_first_opt : 'e. (elt -['e]-> bool) -> t -['e]-> elt option =
    fun p s -> Seq.find_opt_eff p (to_seq s)

  let find_last : 'e. (elt -['e]-> bool) -> t -['e]-> elt =
    fun p s -> Seq.find_eff p (to_rev_seq s)

  let find_last_opt : 'e. (elt -['e]-> bool) -> t -['e]-> elt option =
    fun p s -> Seq.find_opt_eff p (to_rev_seq s)
end
