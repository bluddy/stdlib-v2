include Stdlib.Seq

let rec iter f seq =
  match seq () with
  | Nil -> ()
  | Cons (x, next) ->
      f x;
      iter f next

let iteri f seq =
  let rec aux i seq =
    match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f i x;
        aux (i + 1) next
  in
  aux 0 seq

let rec fold_left f acc seq =
  match seq () with
  | Nil -> acc
  | Cons (x, next) ->
      fold_left f (f acc x) next

let rec for_all p seq =
  match seq () with
  | Nil -> true
  | Cons (x, next) ->
      p x && for_all p next

let rec exists p seq =
  match seq () with
  | Nil -> false
  | Cons (x, next) ->
      p x || exists p next

let rec find p seq =
  match seq () with
  | Nil -> raise Not_found
  | Cons (x, next) ->
      if p x then x else find p next

let rec find_opt p seq =
  match seq () with
  | Nil -> None
  | Cons (x, next) ->
      if p x then Some x else find_opt p next

let find_index p seq =
  let rec aux i seq =
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        if p x then Some i else aux (i + 1) next
  in
  aux 0 seq

let rec find_map f seq =
  match seq () with
  | Nil -> None
  | Cons (x, next) ->
      match f x with
      | Some _ as res -> res
      | None -> find_map f next
