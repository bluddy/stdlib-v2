include Stdlib.Fun

let compose : 'a 'b 'c 'e. ('b -['e]-> 'c) -> ('a -['e]-> 'b) -> 'a -['e]-> 'c =
  fun f g x -> f (g x)

let flip : 'a 'b 'c 'e. ('a -> 'b -['e]-> 'c) -> 'b -> 'a -['e]-> 'c =
  fun f x y -> f y x

let negate : 'a 'e. ('a -['e]-> bool) -> 'a -['e]-> bool =
  fun p x -> not (p x)

let protect : 'a 'e. finally:(unit -> unit) -> (unit -['e]-> 'a) -['e]-> 'a =
  fun ~finally work ->
    let res =
      try work ()
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        (try finally () with exn2 ->
          raise (Finally_raised exn2));
        Printexc.raise_with_backtrace exn bt
    in
    finally ();
    res
