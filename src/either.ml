include Stdlib.Either

let map_left : 'a1 'a2 'b 'e. ('a1 -['e]-> 'a2) -> ('a1, 'b) t -['e]-> ('a2, 'b) t =
  fun f -> function
  | Left v -> Left (f v)
  | Right _ as e -> e

let map_right : 'a 'b1 'b2 'e. ('b1 -['e]-> 'b2) -> ('a, 'b1) t -['e]-> ('a, 'b2) t =
  fun f -> function
  | Left _ as e -> e
  | Right v -> Right (f v)

let map : 'a1 'a2 'b1 'b2 'e.
  left:('a1 -['e]-> 'a2) -> right:('b1 -['e]-> 'b2) -> ('a1, 'b1) t -['e]-> ('a2, 'b2) t =
  fun ~left ~right -> function
  | Left v -> Left (left v)
  | Right v -> Right (right v)

let fold : 'a 'b 'c 'e.
  left:('a -['e]-> 'c) -> right:('b -['e]-> 'c) -> ('a, 'b) t -['e]-> 'c =
  fun ~left ~right -> function
  | Left v -> left v
  | Right v -> right v

let iter : 'a 'b 'e.
  left:('a -['e]-> unit) -> right:('b -['e]-> unit) -> ('a, 'b) t -['e]-> unit =
  fold

let for_all : 'a 'b 'e.
  left:('a -['e]-> bool) -> right:('b -['e]-> bool) -> ('a, 'b) t -['e]-> bool =
  fold

let equal : 'a 'b 'e.
  left:('a -> 'a -['e]-> bool) -> right:('b -> 'b -['e]-> bool) ->
  ('a, 'b) t -> ('a, 'b) t -['e]-> bool =
  fun ~left ~right e1 e2 -> match e1, e2 with
  | Left v1, Left v2 -> left v1 v2
  | Right v1, Right v2 -> right v1 v2
  | Left _, Right _ | Right _, Left _ -> false

let compare : 'a 'b 'e.
  left:('a -> 'a -['e]-> int) -> right:('b -> 'b -['e]-> int) ->
  ('a, 'b) t -> ('a, 'b) t -['e]-> int =
  fun ~left ~right e1 e2 -> match e1, e2 with
  | Left v1, Left v2 -> left v1 v2
  | Right v1, Right v2 -> right v1 v2
  | Left _, Right _ -> (-1)
  | Right _, Left _ -> 1
