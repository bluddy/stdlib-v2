type ('a, 'e) eff = unit -['e]-> ('a, 'e) node
and ('a, 'e) node =
  | Nil
  | Cons of 'a * ('a, 'e) eff

type 'a t = ('a, -[]-) eff

let empty () = Nil

let return x () = Cons (x, empty)

let cons x next () = Cons (x, next)

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

let rec for_all : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> bool =
  fun p seq ->
    match seq () with
    | Nil -> true
    | Cons (x, next) ->
        p x && for_all p next

let rec exists : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> bool =
  fun p seq ->
    match seq () with
    | Nil -> false
    | Cons (x, next) ->
        p x || exists p next

let rec find : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> 'a =
  fun p seq ->
    match seq () with
    | Nil -> raise Not_found
    | Cons (x, next) ->
        if p x then x else find p next

let rec find_opt : 'a 'e. ('a -['e]-> bool) -> 'a t -['e]-> 'a option =
  fun p seq ->
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        if p x then Some x else find_opt p next

let find_index (type a e) (p : a -[e]-> bool) (seq : a t) =
  let rec aux (i : int) (seq : a t) =
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

let once seq =
  let consumed = ref false in
  fun () ->
    if !consumed then raise (Invalid_argument "sequence can only be consumed once")
    else (
      match seq () with
      | Nil -> Nil
      | Cons (x, next) ->
          consumed := true;
          Cons (x, next)
    )

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
