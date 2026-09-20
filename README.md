# stdlib-v2

An effect-polymorphic standard library overlay for OCaml with typed effects.

`stdlib-v2` provides effect-polymorphic versions of standard library higher-order functions (`List.map`, `Array.iter`, `Option.bind`, `Seq.fold_left`, `Fun.protect`, etc.) while remaining 100% backward-compatible with pure code.

## Installation

Within an OCaml `typed-effects` switch:

```bash
opam install stdlib_v2
```

## Usage

Simply open `Stdlib_v2` at the top of your file:

```ocaml
open Stdlib_v2

type _ Effect.t += Log : string -> unit Effect.t

let () =
  let result =
    match
      List.map (fun x ->
        Effect.perform (Log (string_of_int x));
        x * 2
      ) [1; 2; 3]
    with
    | v -> v
    | effect (Log msg), k ->
        print_endline msg;
        Effect.Deep.continue k ()
  in
  assert (result = [2; 4; 6])
```

Or in your `dune` file:

```lisp
(executable
 (name main)
 (libraries stdlib_v2))
```
