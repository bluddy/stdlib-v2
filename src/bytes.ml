include Stdlib.Bytes

let init : 'e. int -> (int -['e]-> char) -['e]-> bytes =
  fun n f ->
    let s = create n in
    for i = 0 to n - 1 do
      unsafe_set s i (f i)
    done;
    s

let iter : 'e. (char -['e]-> unit) -> bytes -['e]-> unit =
  fun f a ->
    for i = 0 to length a - 1 do
      f (unsafe_get a i)
    done

let iteri : 'e. (int -> char -['e]-> unit) -> bytes -['e]-> unit =
  fun f a ->
    for i = 0 to length a - 1 do
      f i (unsafe_get a i)
    done

let map : 'e. (char -['e]-> char) -> bytes -['e]-> bytes =
  fun f s ->
    let l = length s in
    if l = 0 then copy s
    else
      let r = create l in
      for i = 0 to l - 1 do
        unsafe_set r i (f (unsafe_get s i))
      done;
      r

let mapi : 'e. (int -> char -['e]-> char) -> bytes -['e]-> bytes =
  fun f s ->
    let l = length s in
    if l = 0 then copy s
    else
      let r = create l in
      for i = 0 to l - 1 do
        unsafe_set r i (f i (unsafe_get s i))
      done;
      r

let fold_left : 'acc 'e. ('acc -> char -['e]-> 'acc) -> 'acc -> bytes -['e]-> 'acc =
  fun f x a ->
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r (unsafe_get a i)
    done;
    !r

let fold_right : 'acc 'e. (char -> 'acc -['e]-> 'acc) -> bytes -> 'acc -['e]-> 'acc =
  fun f a x ->
    let r = ref x in
    for i = length a - 1 downto 0 do
      r := f (unsafe_get a i) !r
    done;
    !r

let for_all : 'e. (char -['e]-> bool) -> bytes -['e]-> bool =
  fun p s ->
    let n = length s in
    let rec loop i =
      if i = n then true
      else if p (unsafe_get s i) then loop (i + 1)
      else false
    in
    loop 0

let exists : 'e. (char -['e]-> bool) -> bytes -['e]-> bool =
  fun p s ->
    let n = length s in
    let rec loop i =
      if i = n then false
      else if p (unsafe_get s i) then true
      else loop (i + 1)
    in
    loop 0

let to_seq s = Seq.of_stdlib_seq (Stdlib.Bytes.to_seq s)
let to_seqi s = Seq.of_stdlib_seq (Stdlib.Bytes.to_seqi s)

let of_seq : 'e. (char, 'e) Seq.eff -['e]-> bytes =
  fun seq ->
    let buf = Stdlib.Buffer.create 32 in
    let f : char -['e]-> unit = fun c -> Stdlib.Buffer.add_char buf c in
    Seq.iter_eff f seq;
    Stdlib.Buffer.to_bytes buf
