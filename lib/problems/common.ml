include ContainersLabels

let ( <| ) = ( @@ )

module IntMap = Map.Make (Int)

module Monoid = struct
  type 'a t = { identity : 'a; reduce : 'a -> 'a -> 'a }

  let mul = { identity = 1; reduce = ( * ) }
  let add = { identity = 0; reduce = ( + ) }
end

module List = struct
  include CCListLabels

  let pairs t =
    let rec go = function
      | ([] | [ _ ]) as ret -> Error ret
      | x :: y :: rest ->
          Ok ((x, y) :: Result.get_or ~default:[] (go (y :: rest)))
    in
    go t

  let monoid_reduce ~(m : 'a Monoid.t) ls =
    List.fold_left ~init:m.identity ~f:m.reduce ls

  let monoid_map_reduce ~(m : 'a Monoid.t) ~f ls =
    List.fold_on_map ~reduce:m.reduce ~f ~init:m.identity ls
end

module String = struct
  include Stringext
  include CCStringLabels
end

module IntIntOrder = struct
  type t = int * int

  let compare =
    Fun.lexicographic
      (fun (x, _) (x', _) -> Int.compare x x')
      (fun (_, y) (_, y') -> Int.compare y y')
end

let nat_view = function
  | 0 -> `Zero
  | n when n > 0 -> `Succ (n - 1)
  | n -> failwith @@ Printf.sprintf "not a nat: %d" n
