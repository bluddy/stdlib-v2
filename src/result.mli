(** Effect-polymorphic Result module *)

include module type of Stdlib.Result

val map : ('a -['eff]-> 'b) -> ('a, 'e) result -['eff]-> ('b, 'e) result

val map_error : ('e -['eff]-> 'f) -> ('a, 'e) result -['eff]-> ('a, 'f) result

val bind : ('a, 'e) result -> ('a -['eff]-> ('b, 'e) result) -['eff]-> ('b, 'e) result

val fold : ok:('a -['eff]-> 'c) -> error:('e -['eff]-> 'c) -> ('a, 'e) result -['eff]-> 'c

val iter : ('a -['eff]-> unit) -> ('a, 'e) result -['eff]-> unit

val iter_error : ('e -['eff]-> unit) -> ('a, 'e) result -['eff]-> unit

val try_value : ('a, 'e) result -> ('e -['eff]-> ('a, 'e) result) -['eff]-> ('a, 'e) result
