include Stdlib.Result

let map f = function
  | Ok v -> Ok (f v)
  | Error _ as e -> e

let map_error f = function
  | Ok _ as ok -> ok
  | Error e -> Error (f e)

let bind r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e

let fold ~ok ~error = function
  | Ok v -> ok v
  | Error e -> error e

let iter f = function
  | Ok v -> f v
  | Error _ -> ()

let iter_error f = function
  | Ok _ -> ()
  | Error e -> f e

let try_value r f =
  match r with
  | Ok _ as ok -> ok
  | Error e -> f e
