include Stdlib.Hashtbl

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

let to_seq h = Seq.of_stdlib_seq (Stdlib.Hashtbl.to_seq h)
let to_seq_keys h = Seq.of_stdlib_seq (Stdlib.Hashtbl.to_seq_keys h)
let to_seq_values h = Seq.of_stdlib_seq (Stdlib.Hashtbl.to_seq_values h)

let iter : 'a 'b 'e. ('a -> 'b -['e]-> unit) -> ('a, 'b) t -['e]-> unit =
  fun f h -> Seq.iter_eff (fun (k, v) -> f k v) (to_seq h)

let fold : 'a 'b 'acc 'e. ('a -> 'b -> 'acc -['e]-> 'acc) -> ('a, 'b) t -> 'acc -['e]-> 'acc =
  fun f h init -> Seq.fold_left_eff (fun acc (k, v) -> f k v acc) init (to_seq h)

let add_seq : 'a 'b 'e. ('a, 'b) t -> ('a * 'b, 'e) Seq.eff -['e]-> unit =
  fun h seq -> Seq.iter_eff (fun (k, v) -> add h k v) seq

let replace_seq : 'a 'b 'e. ('a, 'b) t -> ('a * 'b, 'e) Seq.eff -['e]-> unit =
  fun h seq -> Seq.iter_eff (fun (k, v) -> replace h k v) seq

let of_seq : 'a 'b 'e. ('a * 'b, 'e) Seq.eff -['e]-> ('a, 'b) t =
  fun seq ->
    let h = create 16 in
    replace_seq h seq;
    h

let filter_map_inplace : 'a 'b 'e. ('a -> 'b -['e]-> 'b option) -> ('a, 'b) t -['e]-> unit =
  fun f h ->
    let pairs = ref [] in
    Seq.iter_eff (fun (k, v) ->
      match f k v with
      | None -> ()
      | Some v' -> pairs := (k, v') :: !pairs
    ) (to_seq h);
    clear h;
    List.iter (fun (k, v) -> add h k v) !pairs

module Make (H : HashedType) : S with type key = H.t and type 'a t = 'a Stdlib.Hashtbl.Make(H).t = struct
  module M = Stdlib.Hashtbl.Make (H)
  include M

  let to_seq h = Seq.of_stdlib_seq (M.to_seq h)
  let to_seq_keys h = Seq.of_stdlib_seq (M.to_seq_keys h)
  let to_seq_values h = Seq.of_stdlib_seq (M.to_seq_values h)

  let iter : 'a 'e. (key -> 'a -['e]-> unit) -> 'a t -['e]-> unit =
    fun f h -> Seq.iter_eff (fun (k, v) -> f k v) (to_seq h)

  let fold : 'a 'acc 'e. (key -> 'a -> 'acc -['e]-> 'acc) -> 'a t -> 'acc -['e]-> 'acc =
    fun f h init -> Seq.fold_left_eff (fun acc (k, v) -> f k v acc) init (to_seq h)

  let add_seq : 'a 'e. 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit =
    fun h seq -> Seq.iter_eff (fun (k, v) -> M.add h k v) seq

  let replace_seq : 'a 'e. 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit =
    fun h seq -> Seq.iter_eff (fun (k, v) -> M.replace h k v) seq

  let of_seq : 'a 'e. (key * 'a, 'e) Seq.eff -['e]-> 'a t =
    fun seq ->
      let h = M.create 16 in
      replace_seq h seq;
      h

  let filter_map_inplace : 'a 'e. (key -> 'a -['e]-> 'a option) -> 'a t -['e]-> unit =
    fun f h ->
      let pairs = ref [] in
      Seq.iter_eff (fun (k, v) ->
        match f k v with
        | None -> ()
        | Some v' -> pairs := (k, v') :: !pairs
      ) (to_seq h);
      M.clear h;
      List.iter (fun (k, v) -> M.add h k v) !pairs
end

module MakeSeeded (H : SeededHashedType) : SeededS with type key = H.t and type 'a t = 'a Stdlib.Hashtbl.MakeSeeded(H).t = struct
  module M = Stdlib.Hashtbl.MakeSeeded (H)
  include M

  let to_seq h = Seq.of_stdlib_seq (M.to_seq h)
  let to_seq_keys h = Seq.of_stdlib_seq (M.to_seq_keys h)
  let to_seq_values h = Seq.of_stdlib_seq (M.to_seq_values h)

  let iter : 'a 'e. (key -> 'a -['e]-> unit) -> 'a t -['e]-> unit =
    fun f h -> Seq.iter_eff (fun (k, v) -> f k v) (to_seq h)

  let fold : 'a 'acc 'e. (key -> 'a -> 'acc -['e]-> 'acc) -> 'a t -> 'acc -['e]-> 'acc =
    fun f h init -> Seq.fold_left_eff (fun acc (k, v) -> f k v acc) init (to_seq h)

  let add_seq : 'a 'e. 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit =
    fun h seq -> Seq.iter_eff (fun (k, v) -> M.add h k v) seq

  let replace_seq : 'a 'e. 'a t -> (key * 'a, 'e) Seq.eff -['e]-> unit =
    fun h seq -> Seq.iter_eff (fun (k, v) -> M.replace h k v) seq

  let of_seq : 'a 'e. (key * 'a, 'e) Seq.eff -['e]-> 'a t =
    fun seq ->
      let h = M.create 16 in
      replace_seq h seq;
      h

  let filter_map_inplace : 'a 'e. (key -> 'a -['e]-> 'a option) -> 'a t -['e]-> unit =
    fun f h ->
      let pairs = ref [] in
      Seq.iter_eff (fun (k, v) ->
        match f k v with
        | None -> ()
        | Some v' -> pairs := (k, v') :: !pairs
      ) (to_seq h);
      M.clear h;
      List.iter (fun (k, v) -> M.add h k v) !pairs
end
