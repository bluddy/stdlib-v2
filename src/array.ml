include Stdlib.Array

let init : 'a 'e. int -> (int -['e]-> 'a) -['e]-> 'a array =
  fun len f ->
    if len < 0 then invalid_arg "Array.init"
    else if len = 0 then [||]
    else
      let res = make len (f 0) in
      for i = 1 to len - 1 do
        set res i (f i)
      done;
      res

let iter : 'a 'e. ('a -['e]-> unit) -> 'a array -['e]-> unit =
  fun f a ->
    for i = 0 to length a - 1 do
      f (get a i)
    done

let iteri : 'a 'e. (int -> 'a -['e]-> unit) -> 'a array -['e]-> unit =
  fun f a ->
    for i = 0 to length a - 1 do
      f i (get a i)
    done

let map : 'a 'b 'e. ('a -['e]-> 'b) -> 'a array -['e]-> 'b array =
  fun f a ->
    let len = length a in
    if len = 0 then [||]
    else
      let res = make len (f (get a 0)) in
      for i = 1 to len - 1 do
        set res i (f (get a i))
      done;
      res

let mapi : 'a 'b 'e. (int -> 'a -['e]-> 'b) -> 'a array -['e]-> 'b array =
  fun f a ->
    let len = length a in
    if len = 0 then [||]
    else
      let res = make len (f 0 (get a 0)) in
      for i = 1 to len - 1 do
        set res i (f i (get a i))
      done;
      res

let fold_left : 'a 'acc 'e. ('acc -> 'a -['e]-> 'acc) -> 'acc -> 'a array -['e]-> 'acc =
  fun f init a ->
    let acc = ref init in
    for i = 0 to length a - 1 do
      acc := f !acc (get a i)
    done;
    !acc

let fold_right : 'a 'acc 'e. ('a -> 'acc -['e]-> 'acc) -> 'a array -> 'acc -['e]-> 'acc =
  fun f a init ->
    let acc = ref init in
    for i = length a - 1 downto 0 do
      acc := f (get a i) !acc
    done;
    !acc

let fold_left_map : 'a 'b 'acc 'e. ('acc -> 'a -['e]-> 'acc * 'b) -> 'acc -> 'a array -['e]-> 'acc * 'b array =
  fun f init a ->
    let len = length a in
    if len = 0 then init, [||]
    else
      let acc, first = f init (get a 0) in
      let res = make len first in
      let acc = ref acc in
      for i = 1 to len - 1 do
        let new_acc, item = f !acc (get a i) in
        acc := new_acc;
        set res i item
      done;
      !acc, res

let iter2 : 'a 'b 'e. ('a -> 'b -['e]-> unit) -> 'a array -> 'b array -['e]-> unit =
  fun f a b ->
    if length a <> length b then invalid_arg "Array.iter2";
    for i = 0 to length a - 1 do
      f (get a i) (get b i)
    done

let map2 : 'a 'b 'c 'e. ('a -> 'b -['e]-> 'c) -> 'a array -> 'b array -['e]-> 'c array =
  fun f a b ->
    let len = length a in
    if len <> length b then invalid_arg "Array.map2";
    if len = 0 then [||]
    else
      let res = make len (f (get a 0) (get b 0)) in
      for i = 1 to len - 1 do
        set res i (f (get a i) (get b i))
      done;
      res

let for_all : 'a 'e. ('a -['e]-> bool) -> 'a array -['e]-> bool =
  fun p a ->
    let len = length a in
    let rec aux i =
      if i >= len then true
      else if p (get a i) then aux (i + 1)
      else false
    in
    aux 0

let exists : 'a 'e. ('a -['e]-> bool) -> 'a array -['e]-> bool =
  fun p a ->
    let len = length a in
    let rec aux i =
      if i >= len then false
      else if p (get a i) then true
      else aux (i + 1)
    in
    aux 0

let for_all2 : 'a 'b 'e. ('a -> 'b -['e]-> bool) -> 'a array -> 'b array -['e]-> bool =
  fun p a b ->
    let len = length a in
    if len <> length b then invalid_arg "Array.for_all2";
    let rec aux i =
      if i >= len then true
      else if p (get a i) (get b i) then aux (i + 1)
      else false
    in
    aux 0

let exists2 : 'a 'b 'e. ('a -> 'b -['e]-> bool) -> 'a array -> 'b array -['e]-> bool =
  fun p a b ->
    let len = length a in
    if len <> length b then invalid_arg "Array.exists2";
    let rec aux i =
      if i >= len then false
      else if p (get a i) (get b i) then true
      else aux (i + 1)
    in
    aux 0

let find_opt : 'a 'e. ('a -['e]-> bool) -> 'a array -['e]-> 'a option =
  fun p a ->
    let len = length a in
    let rec aux i =
      if i >= len then None
      else
        let v = get a i in
        if p v then Some v
        else aux (i + 1)
    in
    aux 0

let find_index : 'a 'e. ('a -['e]-> bool) -> 'a array -['e]-> int option =
  fun p a ->
    let len = length a in
    let rec aux i =
      if i >= len then None
      else if p (get a i) then Some i
      else aux (i + 1)
    in
    aux 0

let find_map : 'a 'b 'e. ('a -['e]-> 'b option) -> 'a array -['e]-> 'b option =
  fun f a ->
    let len = length a in
    let rec aux i =
      if i >= len then None
      else
        match f (get a i) with
        | Some _ as res -> res
        | None -> aux (i + 1)
    in
    aux 0

let find_mapi : 'a 'b 'e. (int -> 'a -['e]-> 'b option) -> 'a array -['e]-> 'b option =
  fun f a ->
    let len = length a in
    let rec aux i =
      if i >= len then None
      else
        match f i (get a i) with
        | Some _ as res -> res
        | None -> aux (i + 1)
    in
    aux 0

let sort : 'a 'e. ('a -> 'a -['e]-> int) -> 'a array -['e]-> unit =
  fun cmp a ->
    let rec qsort lo hi =
      if lo < hi then begin
        let mid = lo + (hi - lo) / 2 in
        let pivot = get a mid in
        let i = ref lo in
        let j = ref hi in
        while !i <= !j do
          while cmp (get a !i) pivot < 0 do incr i done;
          while cmp (get a !j) pivot > 0 do decr j done;
          if !i <= !j then begin
            let tmp = get a !i in
            set a !i (get a !j);
            set a !j tmp;
            incr i;
            decr j;
          end
        done;
        if lo < !j then qsort lo !j;
        if !i < hi then qsort !i hi;
      end
    in
    qsort 0 (length a - 1)

let stable_sort : 'a 'e. ('a -> 'a -['e]-> int) -> 'a array -['e]-> unit =
  fun cmp a ->
    let l = to_list a in
    let l_sorted = List.stable_sort cmp l in
    let rec copy_back i = function
      | [] -> ()
      | h :: t -> set a i h; copy_back (i + 1) t
    in
    copy_back 0 l_sorted

let fast_sort = sort

let to_seq a =
  let len = length a in
  let rec aux i =
    if i >= len then Seq.empty
    else Seq.cons (get a i) (aux (i + 1))
  in
  aux 0

let to_seqi a =
  let len = length a in
  let rec aux i =
    if i >= len then Seq.empty
    else Seq.cons (i, get a i) (aux (i + 1))
  in
  aux 0

let of_seq : 'a 'e. ('a, 'e) Seq.eff -['e]-> 'a array =
  fun seq -> of_list (List.of_seq seq)
