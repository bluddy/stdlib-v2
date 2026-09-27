include Stdlib.Out_channel

let with_open : 'a 'e. (string -> out_channel) -> string -> (out_channel -['e]-> 'a) -['e]-> 'a =
  fun openfun s f ->
    let oc = openfun s in
    Fn.protect ~finally:(fun () -> Stdlib.close_out_noerr oc)
      (fun () -> f oc)

let with_open_bin : 'a 'e. string -> (t -['e]-> 'a) -['e]-> 'a =
  fun s f -> with_open Stdlib.open_out_bin s f

let with_open_text : 'a 'e. string -> (t -['e]-> 'a) -['e]-> 'a =
  fun s f -> with_open Stdlib.open_out s f

let with_open_gen : 'a 'e. open_flag list -> int -> string -> (t -['e]-> 'a) -['e]-> 'a =
  fun flags perm s f -> with_open (Stdlib.open_out_gen flags perm) s f
