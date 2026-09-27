type ('a, 'e) eff = unit -['e]-> ('a, 'e) node
and ('a, 'e) node =
  | Nil
  | Cons of 'a * ('a, 'e) eff

type 'a t = ('a, -[]-) eff

module E : sig
  val empty : ('a, 'e) eff
  val cons : 'a -> ('a, 'e) eff -> ('a, 'e) eff
  val init : int -> (int -['e]-> 'a) -> ('a, 'e) eff
  val unfold : ('b -['e]-> ('a * 'b) option) -> 'b -> ('a, 'e) eff
  val iterate : ('a -['e]-> 'a) -> 'a -> ('a, 'e) eff
  val forever : (unit -['e]-> 'a) -> ('a, 'e) eff
  val take_while : ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff
  val drop_while : ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff
  val of_pure_seq : 'a t -> ('a, 'e) eff
  val of_stdlib_seq : 'a Stdlib.Seq.t -> ('a, 'e) eff
  val repeat : 'a -> ('a, 'e) eff
  val ints : int -> (int, 'e) eff
  val ints_in_range : first:int -> last:int -> (int, 'e) eff
end = struct
  let empty () = Nil
  let cons x next () = Cons (x, next)

  let repeat : 'a 'e. 'a -> ('a, 'e) eff =
    fun x ->
      let rec aux () : ('a, 'e) node = Cons (x, aux) in
      aux

  let ints : 'e. int -> (int, 'e) eff =
    fun i ->
      let rec aux i () : (int, 'e) node = Cons (i, aux (i + 1)) in
      aux i

  let ints_in_range : 'e. first:int -> last:int -> (int, 'e) eff =
    fun ~first ~last ->
      let rec aux i () : (int, 'e) node =
        if last >= i then Cons (i, aux (i + 1))
        else Nil
      in
      aux first

  let of_pure_seq : 'a 'e. 'a t -> ('a, 'e) eff =

    fun s ->
      let rec aux (s : 'a t) () : ('a, 'e) node =
        match s () with
        | Nil -> Nil
        | Cons (x, next) -> Cons (x, aux next)
      in
      aux s

  let of_stdlib_seq : 'a 'e. 'a Stdlib.Seq.t -> ('a, 'e) eff =
    fun s ->
      let rec aux (s : 'a Stdlib.Seq.t) () : ('a, 'e) node =
        match s () with
        | Stdlib.Seq.Nil -> Nil
        | Stdlib.Seq.Cons (x, next) -> Cons (x, aux next)
      in
      aux s

  let rec init n f () =
    if n <= 0 then Nil
    else Cons (f 0, init (n - 1) (fun i -> f (i + 1)))

  let rec unfold f u () =
    match f u with
    | None -> Nil
    | Some (x, u') -> Cons (x, unfold f u')

  let rec iterate f x () =
    Cons (x, iterate f (f x))

  let rec forever f () =
    Cons (f (), forever f)

  let rec take_while p seq () =
    match seq () with
    | Nil -> Nil
    | Cons (x, next) ->
        if p x then Cons (x, take_while p next)
        else Nil

  let rec drop_while p seq () =
    match seq () with
    | Nil -> Nil
    | Cons (x, next) ->
        if p x then drop_while p next ()
        else Cons (x, next)
end

let empty = E.empty
let cons = E.cons
let of_pure_seq = E.of_pure_seq
let of_stdlib_seq = E.of_stdlib_seq
let init = E.init
let unfold = E.unfold
let iterate = E.iterate
let forever = E.forever
let take_while = E.take_while
let drop_while = E.drop_while
let repeat = E.repeat
let ints = E.ints
let ints_in_range = E.ints_in_range
let return x = cons x empty
let singleton = return

let rec of_dispenser f () =
  match f () with
  | None -> Nil
  | Some x -> Cons (x, of_dispenser f)

let to_dispenser (type a e) (it : (a, e) eff) : unit -[e]-> a option =
  let it = ref it in
  fun () ->
    match !it () with
    | Nil -> None
    | Cons (x, next) ->
        it := next;
        Some x

let rec of_list = function
  | [] -> empty
  | x :: xs -> cons x (of_list xs)

let rec to_list seq =
  match seq () with
  | Nil -> []
  | Cons (x, next) -> x :: to_list next

let rec take n seq () =
  if n <= 0 then Nil
  else match seq () with
    | Nil -> Nil
    | Cons (x, next) -> Cons (x, take (n - 1) next)

let rec drop n seq () =
  if n <= 0 then seq ()
  else match seq () with
    | Nil -> Nil
    | Cons (_, next) -> drop (n - 1) next ()

let rec iter : 'a 'e. ('a -['e]-> unit) -> 'a t -['e]-> unit =
  fun f seq ->
    match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f x;
        iter f next

let rec iter_eff : 'a 'e. ('a -['e]-> unit) -> ('a, 'e) eff -['e]-> unit =
  fun f seq ->
    match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f x;
        iter_eff f next

let iteri (type a e) (f : int -> a -[e]-> unit) (seq : a t) =
  let rec aux (i : int) (seq : a t) =
    match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f i x;
        aux (i + 1) next
  in
  aux 0 seq

let iteri_eff (type a e) (f : int -> a -[e]-> unit) (seq : (a, e) eff) =
  let rec aux (i : int) (seq : (a, e) eff) =
    match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f i x;
        aux (i + 1) next
  in
  aux 0 seq

let rec iter2 : 'a 'b 'e. ('a -> 'b -['e]-> unit) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> unit =
  fun f seq1 seq2 ->
    match seq1 (), seq2 () with
    | Cons (x, next1), Cons (y, next2) ->
        f x y;
        iter2 f next1 next2
    | _ -> ()

let rec fold_left : 'a 'acc 'e. ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a t -['e]-> 'acc =
  fun f acc seq ->
    match seq () with
    | Nil -> acc
    | Cons (x, next) ->
        fold_left f (f acc x) next

let rec fold_left_eff : 'a 'acc 'e. ('acc -> 'a -['e]-> 'acc) -> 'acc -> ('a, 'e) eff -['e]-> 'acc =
  fun f acc seq ->
    match seq () with
    | Nil -> acc
    | Cons (x, next) ->
        fold_left_eff f (f acc x) next

let fold_lefti (type a acc e) (f : acc -> int -> a -[e]-> acc) (init : acc) (seq : a t) =
  let rec aux (i : int) (acc : acc) (seq : a t) =
    match seq () with
    | Nil -> acc
    | Cons (x, next) -> aux (i + 1) (f acc i x) next
  in
  aux 0 init seq

let fold_lefti_eff (type a acc e) (f : acc -> int -> a -[e]-> acc) (init : acc) (seq : (a, e) eff) =
  let rec aux (i : int) (acc : acc) (seq : (a, e) eff) =
    match seq () with
    | Nil -> acc
    | Cons (x, next) -> aux (i + 1) (f acc i x) next
  in
  aux 0 init seq

let rec fold_left2 : 'a 'b 'acc 'e. ('acc -> 'a -> 'b -['e]-> 'acc) -> 'acc -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> 'acc =
  fun f acc seq1 seq2 ->
    match seq1 (), seq2 () with
    | Cons (x, next1), Cons (y, next2) ->
        fold_left2 f (f acc x y) next1 next2
    | _ -> acc

let rec for_all : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> bool =
  fun p seq ->
    match seq () with
    | Nil -> true
    | Cons (x, next) ->
        p x && for_all p next

let rec for_all_eff : 'a 'e. ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> bool =
  fun p seq ->
    match seq () with
    | Nil -> true
    | Cons (x, next) ->
        p x && for_all_eff p next

let rec exists : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> bool =
  fun p seq ->
    match seq () with
    | Nil -> false
    | Cons (x, next) ->
        p x || exists p next

let rec exists_eff : 'a 'e. ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> bool =
  fun p seq ->
    match seq () with
    | Nil -> false
    | Cons (x, next) ->
        p x || exists_eff p next

let rec for_all2 : 'a 'b 'e. ('a -> 'b -['e]-> bool) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> bool =
  fun p seq1 seq2 ->
    match seq1 (), seq2 () with
    | Nil, Nil -> true
    | Cons (x, next1), Cons (y, next2) -> p x y && for_all2 p next1 next2
    | _ -> false

let rec exists2 : 'a 'b 'e. ('a -> 'b -['e]-> bool) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> bool =
  fun p seq1 seq2 ->
    match seq1 (), seq2 () with
    | Cons (x, next1), Cons (y, next2) -> p x y || exists2 p next1 next2
    | _ -> false

let rec equal : 'a 'b 'e. ('a -> 'b -['e]-> bool) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> bool =
  fun eq seq1 seq2 ->
    match seq1 (), seq2 () with
    | Nil, Nil -> true
    | Cons (x, next1), Cons (y, next2) -> eq x y && equal eq next1 next2
    | _ -> false

let rec compare : 'a 'b 'e. ('a -> 'b -['e]-> int) -> ('a, 'e) eff -> ('b, 'e) eff -['e]-> int =
  fun cmp seq1 seq2 ->
    match seq1 (), seq2 () with
    | Nil, Nil -> 0
    | Nil, Cons _ -> -1
    | Cons _, Nil -> 1
    | Cons (x, next1), Cons (y, next2) ->
        let c = cmp x y in
        if c <> 0 then c
        else compare cmp next1 next2

let rec find : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> 'a =
  fun p seq ->
    match seq () with
    | Nil -> raise Not_found
    | Cons (x, next) ->
        if p x then x else find p next

let rec find_eff : 'a 'e. ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> 'a =
  fun p seq ->
    match seq () with
    | Nil -> raise Not_found
    | Cons (x, next) ->
        if p x then x else find_eff p next

let rec find_opt : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> 'a option =
  fun p seq ->
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        if p x then Some x else find_opt p next

let rec find_opt_eff : 'a 'e. ('a -['e]-> bool) -> ('a, 'e) eff -['e]-> 'a option =
  fun p seq ->
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        if p x then Some x else find_opt_eff p next

let find_index (type a e) (p : a -[e]-> bool) (seq : a t) =
  let rec aux (i : int) (seq : a t) =
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        if p x then Some i else aux (i + 1) next
  in
  aux 0 seq

let find_index_eff (type a e) (p : a -[e]-> bool) (seq : (a, e) eff) =
  let rec aux (i : int) (seq : (a, e) eff) =
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        if p x then Some i else aux (i + 1) next
  in
  aux 0 seq

let rec find_map : 'a 'b 'e. ('a -['e]-> 'b option) -> 'a t -['e]-> 'b option =
  fun f seq ->
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        match f x with
        | Some _ as res -> res
        | None -> find_map f next

let rec find_map_eff : 'a 'b 'e. ('a -['e]-> 'b option) -> ('a, 'e) eff -['e]-> 'b option =
  fun f seq ->
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        match f x with
        | Some _ as res -> res
        | None -> find_map_eff f next

let find_mapi (type a b e) (f : int -> a -[e]-> b option) (seq : a t) =
  let rec aux (i : int) (seq : a t) =
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        match f i x with
        | Some _ as res -> res
        | None -> aux (i + 1) next
  in
  aux 0 seq

let find_mapi_eff (type a b e) (f : int -> a -[e]-> b option) (seq : (a, e) eff) =
  let rec aux (i : int) (seq : (a, e) eff) =
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        match f i x with
        | Some _ as res -> res
        | None -> aux (i + 1) next
  in
  aux 0 seq

let rec map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> Cons (f x, map f next)

let rec filter p seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) ->
      if p x then Cons (x, filter p next)
      else filter p next ()

let rec filter_map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) ->
      match f x with
      | None -> filter_map f next ()
      | Some y -> Cons (y, filter_map f next)

let rec filteri_aux : 'a 'e. (int -> 'a -['e]-> bool) -> int -> ('a, 'e) eff -> ('a, 'e) eff =
  fun p i seq () ->
    match seq () with
    | Nil -> Nil
    | Cons (x, next) ->
        if p i x then Cons (x, filteri_aux p (i + 1) next)
        else filteri_aux p (i + 1) next ()

let filteri : 'a 'e. (int -> 'a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff =
  fun p seq -> filteri_aux p 0 seq

let rec mapi_aux : 'a 'b 'e. (int -> 'a -['e]-> 'b) -> int -> ('a, 'e) eff -> ('b, 'e) eff =
  fun f i seq () ->
    match seq () with
    | Nil -> Nil
    | Cons (x, next) -> Cons (f i x, mapi_aux f (i + 1) next)

let mapi : 'a 'b 'e. (int -> 'a -['e]-> 'b) -> ('a, 'e) eff -> ('b, 'e) eff =
  fun f seq -> mapi_aux f 0 seq

let rec map2 : 'a 'b 'c 'e. ('a -> 'b -['e]-> 'c) -> ('a, 'e) eff -> ('b, 'e) eff -> ('c, 'e) eff =
  fun f seq1 seq2 () ->
    match seq1 (), seq2 () with
    | Cons (x, next1), Cons (y, next2) -> Cons (f x y, map2 f next1 next2)
    | _ -> Nil

let rec scan : 'a 'b 'e. ('b -> 'a -['e]-> 'b) -> 'b -> ('a, 'e) eff -> ('b, 'e) eff =
  fun f acc seq () ->
    Cons (acc, fun () ->
      match seq () with
      | Nil -> Nil
      | Cons (x, next) -> scan f (f acc x) next ())

let rec zip seq1 seq2 () =
  match seq1 (), seq2 () with
  | Cons (x, next1), Cons (y, next2) -> Cons ((x, y), zip next1 next2)
  | _ -> Nil

let partition : 'a 'e. ('a -['e]-> bool) -> ('a, 'e) eff -> ('a, 'e) eff * ('a, 'e) eff =
  fun p seq ->
    let np : 'a -['e]-> bool = fun x -> not (p x) in
    filter p seq, filter np seq

let partition_map : 'a 'b 'c 'e. ('a -['e]-> ('b, 'c) Either.t) -> ('a, 'e) eff -> ('b, 'e) eff * ('c, 'e) eff =
  fun f seq ->
    let f_left : 'a -['e]-> 'b option = fun x ->
      match f x with
      | Either.Left y -> Some y
      | Either.Right _ -> None
    in
    let f_right : 'a -['e]-> 'c option = fun x ->
      match f x with
      | Either.Left _ -> None
      | Either.Right y -> Some y
    in
    filter_map f_left seq, filter_map f_right seq

let rec append seq1 seq2 () =
  match seq1 () with
  | Nil -> seq2 ()
  | Cons (x, next) -> Cons (x, append next seq2)

let rec concat seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> append x (concat next) ()

let rec flat_map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> append (f x) (flat_map f next) ()

let concat_map = flat_map

let memoize (seq : 'a t) : 'a t =
  let rec memo seq =
    let cell = ref None in
    fun () ->
      match !cell with
      | Some node -> node
      | None ->
          let node =
            match seq () with
            | Nil -> Nil
            | Cons (x, next) -> Cons (x, memo next)
          in
          cell := Some node;
          node
  in
  memo seq

exception Forced_twice

let once seq =
  let consumed = ref false in
  let aux () =
    if !consumed then raise Forced_twice
    else (
      match seq () with
      | Nil -> Nil
      | Cons (x, next) ->
          consumed := true;
          Cons (x, next)
    )
  in
  aux

let is_empty xs =
  match xs () with
  | Nil -> true
  | Cons _ -> false

let uncons xs =
  match xs () with
  | Nil -> None
  | Cons (x, xs) -> Some (x, xs)

let length xs =
  let rec aux acc xs =
    match xs () with
    | Nil -> acc
    | Cons (_, xs) -> aux (acc + 1) xs
  in
  aux 0 xs

let cycle_nonempty xs () =
  let rec tl () = append xs tl () in tl ()

let cycle xs =
  let aux () =
    match xs () with
    | Nil -> Nil
    | Cons (x, xs') -> Cons (x, append xs' (cycle_nonempty xs))
  in
  aux

let delay delayed_seq () = delayed_seq () ()

let rec interleave xs ys () =
  match xs () with
  | Nil -> ys ()
  | Cons (x, xs) -> Cons (x, interleave ys xs)

let rec sorted_merge1l : 'a 'e. ('a -> 'a -['e]-> int) -> 'a -> ('a, 'e) eff -> ('a, 'e) eff -> ('a, 'e) eff =
  fun cmp x xs ys () ->
    match ys () with
    | Nil -> Cons (x, xs)
    | Cons (y, ys) -> sorted_merge1 cmp x xs y ys

and sorted_merge1r : 'a 'e. ('a -> 'a -['e]-> int) -> ('a, 'e) eff -> 'a -> ('a, 'e) eff -> ('a, 'e) eff =
  fun cmp xs y ys () ->
    match xs () with
    | Nil -> Cons (y, ys)
    | Cons (x, xs) -> sorted_merge1 cmp x xs y ys

and sorted_merge1 : 'a 'e. ('a -> 'a -['e]-> int) -> 'a -> ('a, 'e) eff -> 'a -> ('a, 'e) eff -['e]-> ('a, 'e) node =
  fun cmp x xs y ys ->
    if cmp x y <= 0 then
      Cons (x, sorted_merge1r cmp xs y ys)
    else
      Cons (y, sorted_merge1l cmp x xs ys)

let sorted_merge : 'a 'e. ('a -> 'a -['e]-> int) -> ('a, 'e) eff -> ('a, 'e) eff -> ('a, 'e) eff =
  fun cmp xs ys () ->
    match xs (), ys () with
    | Nil, Nil -> Nil
    | Nil, Cons (y, ys') -> Cons (y, ys')
    | Cons (x, xs'), Nil -> Cons (x, xs')
    | Cons (x, xs), Cons (y, ys) -> sorted_merge1 cmp x xs y ys

let rec map_fst xys () =
  match xys () with
  | Nil -> Nil
  | Cons ((x, _), xys) -> Cons (x, map_fst xys)

let rec map_snd xys () =
  match xys () with
  | Nil -> Nil
  | Cons ((_, y), xys) -> Cons (y, map_snd xys)

let unzip xys = map_fst xys, map_snd xys
let split = unzip

let rec group : 'a 'e. ('a -> 'a -['e]-> bool) -> ('a, 'e) eff -> (('a, 'e) eff, 'e) eff =
  fun eq xs () ->
    match xs () with
    | Nil -> Nil
    | Cons (x, xs) ->
        Cons (cons x (take_while (eq x) xs), group eq (drop_while (eq x) xs))

let peel xss = unzip (filter_map uncons xss)

let rec transpose xss () =
  let heads, tails = peel xss in
  if is_empty heads then Nil
  else Cons (heads, transpose tails)

let rec diagonals remainders xss () =
  match xss () with
  | Cons (xs, xss) -> (
      match xs () with
      | Cons (x, xs) ->
          let heads, tails = peel remainders in
          Cons (cons x heads, diagonals (cons xs tails) xss)
      | Nil ->
          let heads, tails = peel remainders in
          Cons (heads, diagonals tails xss))
  | Nil -> transpose remainders ()

let map_product : 'a 'b 'c 'e. ('a -> 'b -['e]-> 'c) -> ('a, 'e) eff -> ('b, 'e) eff -> ('c, 'e) eff =
  fun f xs ys ->
    concat (diagonals empty (map (fun x -> map (fun y -> f x y) ys) xs))

let product xs ys = map_product (fun x y -> (x, y)) xs ys

let rec to_stdlib (seq : 'a t) : 'a Stdlib.Seq.t =
  fun () ->
    match seq () with
    | Nil -> Stdlib.Seq.Nil
    | Cons (x, next) -> Stdlib.Seq.Cons (x, to_stdlib next)

let rec of_stdlib (seq : 'a Stdlib.Seq.t) : 'a t =
  fun () ->
    match seq () with
    | Stdlib.Seq.Nil -> Nil
    | Stdlib.Seq.Cons (x, next) -> Cons (x, of_stdlib next)
