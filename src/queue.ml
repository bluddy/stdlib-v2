include Stdlib.Queue

let to_seq q = Seq.of_stdlib_seq (Stdlib.Queue.to_seq q)

let iter : 'a 'e. ('a -['e]-> unit) -> 'a t -['e]-> unit =
  fun f q -> Seq.iter_eff f (to_seq q)

let fold : 'a 'acc 'e. ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc =
  fun f acc q -> Seq.fold_left_eff f acc (to_seq q)

let add_seq : 'a 'e. 'a t -> ('a, 'e) Seq.eff -['e]-> unit =
  fun q seq -> Seq.iter_eff (fun x -> add x q) seq

let of_seq : 'a 'e. ('a, 'e) Seq.eff -['e]-> 'a t =
  fun seq ->
    let q = create () in
    add_seq q seq;
    q
