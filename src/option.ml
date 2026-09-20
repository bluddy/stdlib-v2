include Stdlib.Option

let map f = function
  | None -> None
  | Some v -> Some (f v)

let bind opt f =
  match opt with
  | None -> None
  | Some v -> f v

let iter f = function
  | None -> ()
  | Some v -> f v

let fold ~none ~some = function
  | None -> none
  | Some v -> some v

let for_all p = function
  | None -> true
  | Some v -> p v

let exists p = function
  | None -> false
  | Some v -> p v

let filter p = function
  | Some v as o when p v -> o
  | _ -> None
