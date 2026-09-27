module List : module type of List
module Array : module type of Array
module Option : module type of Option
module Seq : module type of Seq
module Result : module type of Result
module Fun : module type of Fn
module String : module type of String
module Bytes : module type of Bytes
module In_channel : module type of In_channel
module Out_channel : module type of Out_channel
module Either : module type of Either
module Buffer : module type of Buffer
module Queue : module type of Queue
module Stack : module type of Stack
module Hashtbl : module type of Hashtbl
module Map : module type of Map
module Set : module type of Set

include module type of Stdlib
  with module List := Stdlib.List
  and module Array := Stdlib.Array
  and module Option := Stdlib.Option
  and module Seq := Stdlib.Seq
  and module Result := Stdlib.Result
  and module Fun := Stdlib.Fun
  and module String := Stdlib.String
  and module Bytes := Stdlib.Bytes
  and module In_channel := Stdlib.In_channel
  and module Out_channel := Stdlib.Out_channel
  and module Either := Stdlib.Either
  and module Buffer := Stdlib.Buffer
  and module Queue := Stdlib.Queue
  and module Stack := Stdlib.Stack
  and module Hashtbl := Stdlib.Hashtbl
  and module Map := Stdlib.Map
  and module Set := Stdlib.Set
  and type ('a, 'b) result = ('a, 'b) Stdlib.result
  and type open_flag = Stdlib.open_flag
  and type fpclass = Stdlib.fpclass
  and type in_channel = Stdlib.in_channel
  and type out_channel = Stdlib.out_channel
