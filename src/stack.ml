include Stdlib.Stack

let to_seq s = Seq.of_stdlib_seq (Stdlib.Stack.to_seq s)

let iter : 'a 'e. ('a -['e]-> unit) -> 'a t -['e]-> unit =
  fun f s -> Seq.iter_eff f (to_seq s)

let fold : 'a 'acc 'e. ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc =
  fun f acc s -> Seq.fold_left_eff f acc (to_seq s)

let add_seq : 'a 'e. 'a t -> ('a, 'e) Seq.eff -['e]-> unit =
  fun s seq -> Seq.iter_eff (fun x -> push x s) seq

let of_seq : 'a 'e. ('a, 'e) Seq.eff -['e]-> 'a t =
  fun seq ->
    let s = create () in
    add_seq s seq;
    s
