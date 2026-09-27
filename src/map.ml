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

module Make (Ord : OrderedType) : S with type key = Ord.t and type !+'a t = 'a Stdlib.Map.Make(Ord).t = struct
  module M = Stdlib.Map.Make(Ord)

  type key = M.key
  type 'a t = 'a M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let is_singleton = M.is_singleton
  let singleton_to_binding = M.singleton_to_binding
  let mem = M.mem
  let add = M.add
  let add_to_list = M.add_to_list
  let singleton = M.singleton
  let remove = M.remove
  let cardinal = M.cardinal
  let bindings = M.bindings
  let min_binding = M.min_binding
  let min_binding_opt = M.min_binding_opt
  let max_binding = M.max_binding
  let max_binding_opt = M.max_binding_opt
  let choose = M.choose
  let choose_opt = M.choose_opt
  let find = M.find
  let find_opt = M.find_opt
  let split = M.split
  let to_list = M.to_list
  let of_list = M.of_list

  let to_seq m = Seq.of_stdlib_seq (M.to_seq m)
  let to_rev_seq m = Seq.of_stdlib_seq (M.to_rev_seq m)
  let to_seq_from k m = Seq.of_stdlib_seq (M.to_seq_from k m)

  let add_seq : 'a 'e. (key * 'a, 'e) Seq.eff -> 'a t -['e]-> 'a t =
    fun seq m ->
      let f : 'a t -> key * 'a -['e]-> 'a t = fun acc (k, v) -> M.add k v acc in
      Seq.fold_left_eff f m seq

  let of_seq : 'a 'e. (key * 'a, 'e) Seq.eff -['e]-> 'a t =
    fun seq -> add_seq seq M.empty

  let iter : 'a 'e. (key -> 'a -['e]-> unit) -> 'a t -['e]-> unit =
    fun f m ->
      let f' : key * 'a -['e]-> unit = fun (k, v) -> f k v in
      Seq.iter_eff f' (to_seq m)

  let fold : 'a 'acc 'e. (key -> 'a -> 'acc -['e]-> 'acc) -> 'a t -> 'acc -['e]-> 'acc =
    fun f m init ->
      let f' : 'acc -> key * 'a -['e]-> 'acc = fun acc (k, v) -> f k v acc in
      Seq.fold_left_eff f' init (to_seq m)

  let map : 'a 'b 'e. ('a -['e]-> 'b) -> 'a t -['e]-> 'b t =
    fun f m ->
      let f' : 'b t -> key * 'a -['e]-> 'b t = fun acc (k, v) -> M.add k (f v) acc in
      Seq.fold_left_eff f' M.empty (to_seq m)

  let mapi : 'a 'b 'e. (key -> 'a -['e]-> 'b) -> 'a t -['e]-> 'b t =
    fun f m ->
      let f' : 'b t -> key * 'a -['e]-> 'b t = fun acc (k, v) -> M.add k (f k v) acc in
      Seq.fold_left_eff f' M.empty (to_seq m)

  let filter : 'a 'e. (key -> 'a -['e]-> bool) -> 'a t -['e]-> 'a t =
    fun p m ->
      let f' : 'a t -> key * 'a -['e]-> 'a t = fun acc (k, v) ->
        if p k v then M.add k v acc else acc
      in
      Seq.fold_left_eff f' M.empty (to_seq m)

  let filter_map : 'a 'b 'e. (key -> 'a -['e]-> 'b option) -> 'a t -['e]-> 'b t =
    fun f m ->
      let f' : 'b t -> key * 'a -['e]-> 'b t = fun acc (k, v) ->
        match f k v with
        | None -> acc
        | Some v' -> M.add k v' acc
      in
      Seq.fold_left_eff f' M.empty (to_seq m)

  let partition : 'a 'e. (key -> 'a -['e]-> bool) -> 'a t -['e]-> 'a t * 'a t =
    fun p m ->
      let f' : 'a t * 'a t -> key * 'a -['e]-> 'a t * 'a t = fun (m1, m2) (k, v) ->
        if p k v then (M.add k v m1, m2) else (m1, M.add k v m2)
      in
      Seq.fold_left_eff f' (M.empty, M.empty) (to_seq m)

  let for_all : 'a 'e. (key -> 'a -['e]-> bool) -> 'a t -['e]-> bool =
    fun p m ->
      let p' : key * 'a -['e]-> bool = fun (k, v) -> p k v in
      Seq.for_all_eff p' (to_seq m)

  let exists : 'a 'e. (key -> 'a -['e]-> bool) -> 'a t -['e]-> bool =
    fun p m ->
      let p' : key * 'a -['e]-> bool = fun (k, v) -> p k v in
      Seq.exists_eff p' (to_seq m)

  let update : 'a 'e. key -> ('a option -['e]-> 'a option) -> 'a t -['e]-> 'a t =
    fun k f m ->
      let opt = M.find_opt k m in
      match f opt with
      | None -> M.remove k m
      | Some v -> M.add k v m

  let merge : 'a 'b 'c 'e. (key -> 'a option -> 'b option -['e]-> 'c option) -> 'a t -> 'b t -['e]-> 'c t =
    fun f m1 m2 ->
      let s1 = to_seq m1 in
      let s2 = to_seq m2 in
      let rec loop s1 s2 acc =
        match s1 (), s2 () with
        | Seq.Nil, Seq.Nil -> acc
        | Seq.Cons ((k1, v1), rest1), Seq.Nil ->
            let acc' = match f k1 (Some v1) None with None -> acc | Some v -> M.add k1 v acc in
            loop rest1 (fun () -> Seq.Nil) acc'
        | Seq.Nil, Seq.Cons ((k2, v2), rest2) ->
            let acc' = match f k2 None (Some v2) with None -> acc | Some v -> M.add k2 v acc in
            loop (fun () -> Seq.Nil) rest2 acc'
        | Seq.Cons ((k1, v1), rest1), Seq.Cons ((k2, v2), rest2) ->
            let c = Ord.compare k1 k2 in
            if c = 0 then
              let acc' = match f k1 (Some v1) (Some v2) with None -> acc | Some v -> M.add k1 v acc in
              loop rest1 rest2 acc'
            else if c < 0 then
              let acc' = match f k1 (Some v1) None with None -> acc | Some v -> M.add k1 v acc in
              loop rest1 (fun () -> Seq.Cons ((k2, v2), rest2)) acc'
            else
              let acc' = match f k2 None (Some v2) with None -> acc | Some v -> M.add k2 v acc in
              loop (fun () -> Seq.Cons ((k1, v1), rest1)) rest2 acc'
      in
      loop s1 s2 M.empty

  let union : 'a 'e. (key -> 'a -> 'a -['e]-> 'a option) -> 'a t -> 'a t -['e]-> 'a t =
    fun f m1 m2 ->
      merge (fun k v1 v2 ->
        match v1, v2 with
        | None, None -> None
        | Some v, None | None, Some v -> Some v
        | Some v1, Some v2 -> f k v1 v2
      ) m1 m2

  let find_first : 'a 'e. (key -['e]-> bool) -> 'a t -['e]-> key * 'a =
    fun p m ->
      Seq.find_eff (fun (k, _) -> p k) (to_seq m)

  let find_first_opt : 'a 'e. (key -['e]-> bool) -> 'a t -['e]-> (key * 'a) option =
    fun p m ->
      Seq.find_opt_eff (fun (k, _) -> p k) (to_seq m)

  let find_last : 'a 'e. (key -['e]-> bool) -> 'a t -['e]-> key * 'a =
    fun p m ->
      Seq.find_eff (fun (k, _) -> p k) (to_rev_seq m)

  let find_last_opt : 'a 'e. (key -['e]-> bool) -> 'a t -['e]-> (key * 'a) option =
    fun p m ->
      Seq.find_opt_eff (fun (k, _) -> p k) (to_rev_seq m)

  let equal : 'a 'e. ('a -> 'a -['e]-> bool) -> 'a t -> 'a t -['e]-> bool =
    fun eq m1 m2 ->
      if M.cardinal m1 <> M.cardinal m2 then false
      else
        let s1 = to_seq m1 in
        let s2 = to_seq m2 in
        let rec loop s1 s2 =
          match s1 (), s2 () with
          | Seq.Nil, Seq.Nil -> true
          | Seq.Cons ((k1, v1), rest1), Seq.Cons ((k2, v2), rest2) ->
              if Ord.compare k1 k2 = 0 && eq v1 v2 then loop rest1 rest2
              else false
          | _ -> false
        in
        loop s1 s2

  let compare : 'a 'e. ('a -> 'a -['e]-> int) -> 'a t -> 'a t -['e]-> int =
    fun cmp m1 m2 ->
      let s1 = to_seq m1 in
      let s2 = to_seq m2 in
      let rec loop s1 s2 =
        match s1 (), s2 () with
        | Seq.Nil, Seq.Nil -> 0
        | Seq.Nil, _ -> (-1)
        | _, Seq.Nil -> 1
        | Seq.Cons ((k1, v1), rest1), Seq.Cons ((k2, v2), rest2) ->
            let c = Ord.compare k1 k2 in
            if c <> 0 then c
            else
              let c = cmp v1 v2 in
              if c <> 0 then c
              else loop rest1 rest2
      in
      loop s1 s2
end
