include Stdlib.In_channel

let with_open : 'a 'e. (string -> in_channel) -> string -> (in_channel -['e]-> 'a) -['e]-> 'a =
  fun openfun s f ->
    let ic = openfun s in
    Fn.protect ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () -> f ic)

let with_open_bin : 'a 'e. string -> (t -['e]-> 'a) -['e]-> 'a =
  fun s f -> with_open Stdlib.open_in_bin s f

let with_open_text : 'a 'e. string -> (t -['e]-> 'a) -['e]-> 'a =
  fun s f -> with_open Stdlib.open_in s f

let with_open_gen : 'a 'e. open_flag list -> int -> string -> (t -['e]-> 'a) -['e]-> 'a =
  fun flags perm s f -> with_open (Stdlib.open_in_gen flags perm) s f

let rec fold_lines : 'acc 'e. ('acc -> string -['e]-> 'acc) -> 'acc -> t -['e]-> 'acc =
  fun f accu ic ->
    match Stdlib.input_line ic with
    | line -> fold_lines f (f accu line) ic
    | exception End_of_file -> accu
