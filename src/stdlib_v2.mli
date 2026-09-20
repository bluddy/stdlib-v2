module List : module type of List
module Array : module type of Array
module Option : module type of Option
module Seq : module type of Seq
module Result : module type of Result
module Fun : module type of Fn

include module type of Stdlib
  with module List := Stdlib.List
  and module Array := Stdlib.Array
  and module Option := Stdlib.Option
  and module Seq := Stdlib.Seq
  and module Result := Stdlib.Result
  and module Fun := Stdlib.Fun
  and type ('a, 'b) result = ('a, 'b) Stdlib.result
  and type open_flag = Stdlib.open_flag
  and type fpclass = Stdlib.fpclass
  and type in_channel = Stdlib.in_channel
  and type out_channel = Stdlib.out_channel
