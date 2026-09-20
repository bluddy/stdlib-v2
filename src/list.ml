include Stdlib.List

let rec init_aux i last f =
  if i > last then []
  else
    let r = f i in
    r :: init_aux (i + 1) last f

let init : 'a 'e. int -> (int -['e]-> 'a) -['e]-> 'a list =
  fun len f ->
    if len < 0 then invalid_arg "List.init" else
    init_aux 0 (len - 1) f

let rec iter : 'a 'e. ('a -['e]-> unit) -> 'a list -['e]-> unit =
  fun f -> function
  | [] -> ()
  | a :: l -> f a; iter f l

let iteri : 'a 'e. (int -> 'a -['e]-> unit) -> 'a list -['e]-> unit =
  fun f l ->
    let rec aux i = function
      | [] -> ()
      | a :: l -> f i a; aux (i + 1) l
    in
    aux 0 l

let rec map : 'a 'b 'e. ('a -['e]-> 'b) -> 'a list -['e]-> 'b list =
  fun f -> function
  | [] -> []
  | a :: l ->
      let r = f a in
      r :: map f l

let mapi : 'a 'b 'e. (int -> 'a -['e]-> 'b) -> 'a list -['e]-> 'b list =
  fun f l ->
    let rec aux i = function
      | [] -> []
      | a :: l ->
          let r = f i a in
          r :: aux (i + 1) l
    in
    aux 0 l

let rev_map : 'a 'b 'e. ('a -['e]-> 'b) -> 'a list -['e]-> 'b list =
  fun f l ->
    let rec rmap_f accu = function
      | [] -> accu
      | a :: l -> rmap_f (f a :: accu) l
    in
    rmap_f [] l

let rec filter_map : 'a 'b 'e. ('a -['e]-> 'b option) -> 'a list -['e]-> 'b list =
  fun f -> function
  | [] -> []
  | a :: l ->
      match f a with
      | None -> filter_map f l
      | Some r -> r :: filter_map f l

let filter_mapi : 'a 'b 'e. (int -> 'a -['e]-> 'b option) -> 'a list -['e]-> 'b list =
  fun f l ->
    let rec aux i = function
      | [] -> []
      | a :: l ->
          match f i a with
          | None -> aux (i + 1) l
          | Some r -> r :: aux (i + 1) l
    in
    aux 0 l

let rec concat_map : 'a 'b 'e. ('a -['e]-> 'b list) -> 'a list -['e]-> 'b list =
  fun f -> function
  | [] -> []
  | a :: l ->
      let r = f a in
      r @ concat_map f l

let rec fold_left : 'a 'acc 'e. ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a list -['e]-> 'acc =
  fun f accu -> function
  | [] -> accu
  | a :: l -> fold_left f (f accu a) l

let rec fold_right : 'a 'acc 'e. ('a -> 'acc -['e]-> 'acc) -> 'a list -> 'acc -['e]-> 'acc =
  fun f l accu ->
    match l with
    | [] -> accu
    | a :: l -> f a (fold_right f l accu)

let fold_left_map : 'a 'b 'acc 'e. ('acc -> 'a -['e]-> 'acc * 'b) -> 'acc -> 'a list -['e]-> 'acc * 'b list =
  fun f accu l ->
    let rec aux accu l_accu = function
      | [] -> accu, rev l_accu
      | a :: l ->
          let accu, b = f accu a in
          aux accu (b :: l_accu) l
    in
    aux accu [] l

let rec iter2 : 'a 'b 'e. ('a -> 'b -['e]-> unit) -> 'a list -> 'b list -['e]-> unit =
  fun f l1 l2 ->
    match l1, l2 with
    | [], [] -> ()
    | a1 :: l1, a2 :: l2 -> f a1 a2; iter2 f l1 l2
    | _, _ -> invalid_arg "List.iter2"

let rec map2 : 'a 'b 'c 'e. ('a -> 'b -['e]-> 'c) -> 'a list -> 'b list -['e]-> 'c list =
  fun f l1 l2 ->
    match l1, l2 with
    | [], [] -> []
    | a1 :: l1, a2 :: l2 ->
        let r = f a1 a2 in
        r :: map2 f l1 l2
    | _, _ -> invalid_arg "List.map2"

let rev_map2 : 'a 'b 'c 'e. ('a -> 'b -['e]-> 'c) -> 'a list -> 'b list -['e]-> 'c list =
  fun f l1 l2 ->
    let rec rmap2_f accu l1 l2 =
      match l1, l2 with
      | [], [] -> accu
      | a1 :: l1, a2 :: l2 -> rmap2_f (f a1 a2 :: accu) l1 l2
      | _, _ -> invalid_arg "List.rev_map2"
    in
    rmap2_f [] l1 l2

let rec fold_left2 : 'a 'b 'acc 'e. ('acc -> 'a -> 'b -['e]-> 'acc) -> 'acc -> 'a list -> 'b list -['e]-> 'acc =
  fun f accu l1 l2 ->
    match l1, l2 with
    | [], [] -> accu
    | a1 :: l1, a2 :: l2 -> fold_left2 f (f accu a1 a2) l1 l2
    | _, _ -> invalid_arg "List.fold_left2"

let rec fold_right2 : 'a 'b 'acc 'e. ('a -> 'b -> 'acc -['e]-> 'acc) -> 'a list -> 'b list -> 'acc -['e]-> 'acc =
  fun f l1 l2 accu ->
    match l1, l2 with
    | [], [] -> accu
    | a1 :: l1, a2 :: l2 -> f a1 a2 (fold_right2 f l1 l2 accu)
    | _, _ -> invalid_arg "List.fold_right2"

let rec for_all : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> bool =
  fun p -> function
  | [] -> true
  | a :: l -> p a && for_all p l

let rec exists : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> bool =
  fun p -> function
  | [] -> false
  | a :: l -> p a || exists p l

let rec for_all2 : 'a 'b 'e. ('a -> 'b -['e]-> bool) -> 'a list -> 'b list -['e]-> bool =
  fun p l1 l2 ->
    match l1, l2 with
    | [], [] -> true
    | a1 :: l1, a2 :: l2 -> p a1 a2 && for_all2 p l1 l2
    | _, _ -> invalid_arg "List.for_all2"

let rec exists2 : 'a 'b 'e. ('a -> 'b -['e]-> bool) -> 'a list -> 'b list -['e]-> bool =
  fun p l1 l2 ->
    match l1, l2 with
    | [], [] -> false
    | a1 :: l1, a2 :: l2 -> p a1 a2 || exists2 p l1 l2
    | _, _ -> invalid_arg "List.exists2"

let rec find : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> 'a =
  fun p -> function
  | [] -> raise Not_found
  | a :: l -> if p a then a else find p l

let rec find_opt : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> 'a option =
  fun p -> function
  | [] -> None
  | a :: l -> if p a then Some a else find_opt p l

let find_index : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> int option =
  fun p l ->
    let rec aux i = function
      | [] -> None
      | a :: l -> if p a then Some i else aux (i + 1) l
    in
    aux 0 l

let rec find_map : 'a 'b 'e. ('a -['e]-> 'b option) -> 'a list -['e]-> 'b option =
  fun f -> function
  | [] -> None
  | a :: l ->
      match f a with
      | Some _ as r -> r
      | None -> find_map f l

let find_mapi : 'a 'b 'e. (int -> 'a -['e]-> 'b option) -> 'a list -['e]-> 'b option =
  fun f l ->
    let rec aux i = function
      | [] -> None
      | a :: l ->
          match f i a with
          | Some _ as r -> r
          | None -> aux (i + 1) l
    in
    aux 0 l

let rec filter : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> 'a list =
  fun p -> function
  | [] -> []
  | a :: l -> if p a then a :: filter p l else filter p l

let find_all = filter

let filteri : 'a 'e. (int -> 'a -['e]-> bool) -> 'a list -['e]-> 'a list =
  fun p l ->
    let rec aux i = function
      | [] -> []
      | a :: l -> if p i a then a :: aux (i + 1) l else aux (i + 1) l
    in
    aux 0 l

let rec take_while : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> 'a list =
  fun p -> function
  | a :: l when p a -> a :: take_while p l
  | _ -> []

let rec drop_while : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> 'a list =
  fun p -> function
  | a :: l when p a -> drop_while p l
  | l -> l

let rec partition : 'a 'e. ('a -['e]-> bool) -> 'a list -['e]-> 'a list * 'a list =
  fun p -> function
  | [] -> [], []
  | a :: l ->
      let yes, no = partition p l in
      if p a then a :: yes, no else yes, a :: no

let rec partition_map : 'a 'b 'c 'e. ('a -['e]-> ('b, 'c) Either.t) -> 'a list -['e]-> 'b list * 'c list =
  fun f -> function
  | [] -> [], []
  | a :: l ->
      let l1, l2 = partition_map f l in
      match f a with
      | Either.Left y1 -> y1 :: l1, l2
      | Either.Right y2 -> l1, y2 :: l2

let rec merge : 'a 'e. ('a -> 'a -['e]-> int) -> 'a list -> 'a list -['e]-> 'a list =
  fun cmp l1 l2 ->
    match l1, l2 with
    | [], l -> l
    | l, [] -> l
    | h1 :: t1, h2 :: t2 ->
        if cmp h1 h2 <= 0 then
          h1 :: merge cmp t1 l2
        else
          h2 :: merge cmp l1 t2

let rec split_half acc len l =
  if len = 0 then rev acc, l
  else match l with
  | [] -> rev acc, []
  | h :: t -> split_half (h :: acc) (len - 1) t

let rec sort : 'a 'e. ('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list =
  fun cmp -> function
  | [] -> []
  | [_] as l -> l
  | l ->
      let len = length l in
      let l1, l2 = split_half [] (len / 2) l in
      merge cmp (sort cmp l1) (sort cmp l2)

let stable_sort = sort
let fast_sort = sort

let sort_uniq : 'a 'e. ('a -> 'a -['e]-> int) -> 'a list -['e]-> 'a list =
  fun cmp l ->
    let sorted = sort cmp l in
    let rec uniq = function
      | [] -> []
      | [x] -> [x]
      | x :: (y :: _ as rest) ->
          if cmp x y = 0 then uniq rest else x :: uniq rest
    in
    uniq sorted
