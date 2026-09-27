include Stdlib.String

let init : 'e. int -> (int -['e]-> char) -['e]-> string =
  fun n f ->
    if n < 0 then invalid_arg "String.init";
    let b = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set b i (f i)
    done;
    Bytes.unsafe_to_string b

let iter : 'e. (char -['e]-> unit) -> string -['e]-> unit =
  fun f s ->
    for i = 0 to length s - 1 do
      f (unsafe_get s i)
    done

let iteri : 'e. (int -> char -['e]-> unit) -> string -['e]-> unit =
  fun f s ->
    for i = 0 to length s - 1 do
      f i (unsafe_get s i)
    done

let map : 'e. (char -['e]-> char) -> string -['e]-> string =
  fun f s ->
    let l = length s in
    if l = 0 then ""
    else
      let r = Bytes.create l in
      for i = 0 to l - 1 do
        Bytes.set r i (f (unsafe_get s i))
      done;
      Bytes.unsafe_to_string r

let mapi : 'e. (int -> char -['e]-> char) -> string -['e]-> string =
  fun f s ->
    let l = length s in
    if l = 0 then ""
    else
      let r = Bytes.create l in
      for i = 0 to l - 1 do
        Bytes.set r i (f i (unsafe_get s i))
      done;
      Bytes.unsafe_to_string r

let fold_left : 'acc 'e. ('acc -> char -['e]-> 'acc) -> 'acc -> string -['e]-> 'acc =
  fun f x a ->
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r (unsafe_get a i)
    done;
    !r

let fold_right : 'acc 'e. (char -> 'acc -['e]-> 'acc) -> string -> 'acc -['e]-> 'acc =
  fun f a x ->
    let r = ref x in
    for i = length a - 1 downto 0 do
      r := f (unsafe_get a i) !r
    done;
    !r

let for_all : 'e. (char -['e]-> bool) -> string -['e]-> bool =
  fun p s ->
    let n = length s in
    let rec loop i =
      if i = n then true
      else if p (unsafe_get s i) then loop (i + 1)
      else false
    in
    loop 0

let exists : 'e. (char -['e]-> bool) -> string -['e]-> bool =
  fun p s ->
    let n = length s in
    let rec loop i =
      if i = n then false
      else if p (unsafe_get s i) then loop (i + 1)
      else loop (i + 1)
    in
    loop 0

let take_first_while : 'e. (char -['e]-> bool) -> string -['e]-> string =
  fun sat s ->
    let len = length s in
    let rec loop i =
      if i = len then s
      else if sat (unsafe_get s i) then loop (i + 1)
      else sub s 0 i
    in
    loop 0

let drop_first_while : 'e. (char -['e]-> bool) -> string -['e]-> string =
  fun sat s ->
    let len = length s in
    let rec loop i =
      if i = len then ""
      else if sat (unsafe_get s i) then loop (i + 1)
      else sub s i (len - i)
    in
    loop 0

let cut_first_while : 'e. (char -['e]-> bool) -> string -['e]-> string * string =
  fun sat s ->
    let len = length s in
    let rec loop i =
      if i = len then (s, "")
      else if sat (unsafe_get s i) then loop (i + 1)
      else (sub s 0 i, sub s i (len - i))
    in
    loop 0

let take_last_while : 'e. (char -['e]-> bool) -> string -['e]-> string =
  fun sat s ->
    let len = length s in
    let rec loop i =
      if i < 0 then s
      else if sat (unsafe_get s i) then loop (i - 1)
      else
        let j = i + 1 in
        sub s j (len - j)
    in
    loop (len - 1)

let drop_last_while : 'e. (char -['e]-> bool) -> string -['e]-> string =
  fun sat s ->
    let len = length s in
    let rec loop i =
      if i < 0 then ""
      else if sat (unsafe_get s i) then loop (i - 1)
      else sub s 0 (i + 1)
    in
    loop (len - 1)

let cut_last_while : 'e. (char -['e]-> bool) -> string -['e]-> string * string =
  fun sat s ->
    let len = length s in
    let rec loop i =
      if i < 0 then ("", s)
      else if sat (unsafe_get s i) then loop (i - 1)
      else
        let j = i + 1 in
        (sub s 0 j, sub s j (len - j))
    in
    loop (len - 1)

let find_first_index : 'e. (char -['e]-> bool) -> ?start:int -> string -['e]-> int option =
  fun sat ?(start = 0) s ->
    let len = length s in
    if not (0 <= start && start <= len) then
      invalid_arg (concat "" ["start: "; string_of_int start; " not in range [0;"; string_of_int len; "]"])
    else
      let rec loop i =
        if i = len then None
        else if sat (unsafe_get s i) then Some i
        else loop (i + 1)
      in
      loop start

let find_last_index : 'e. (char -['e]-> bool) -> ?start:int -> string -['e]-> int option =
  fun sat ?start s ->
    let len = length s in
    let st = match start with None -> len | Some s -> s in
    if not (0 <= st && st <= len) then
      invalid_arg (concat "" ["start: "; string_of_int st; " not in range [0;"; string_of_int len; "]"])
    else
      let rec loop i =
        if i < 0 then None
        else if sat (unsafe_get s i) then Some i
        else loop (i - 1)
      in
      loop (if st = len then len - 1 else st)

let to_seq s = Seq.of_stdlib_seq (Stdlib.String.to_seq s)
let to_seqi s = Seq.of_stdlib_seq (Stdlib.String.to_seqi s)

let of_seq : 'e. (char, 'e) Seq.eff -['e]-> string =
  fun seq ->
    let buf = Stdlib.Buffer.create 32 in
    let f : char -['e]-> unit = fun c -> Stdlib.Buffer.add_char buf c in
    Seq.iter_eff f seq;
    Stdlib.Buffer.contents buf
