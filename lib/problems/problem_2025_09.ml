open! Common

let year = 2025
let day = 9
let example = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3\n"

(* Some of the tiles are red; the Elves would like to find the largest rectangle that uses red
   tiles for two of its opposite corners. They even have a list of where the red tiles are
   located in the grid (your puzzle input). *)

(* Ultimately, the largest rectangle you can make in this example has area 50. One way to do
this is between 2,5 and 11,1:

   ..............
   ..OOOOOOOOOO..
   ..OOOOOOOOOO..
   ..OOOOOOOOOO..
   ..OOOOOOOOOO..
   ..OOOOOOOOOO..
   ..............
   .........#.#..
   ..............
*)

module AreaHeap = Heap.Make_from_compare (struct
  type t = Point2d.t * Point2d.t

  let compare =
    let rev_dist_compare l r =
      Int.compare (Fun.uncurry Point2d.area r) (Fun.uncurry Point2d.area l)
    in
    Fun.lexicographic rev_dist_compare
    @@ Pair.compare Point2d.compare Point2d.compare
end)

let parse str =
  str |> String.lines
  |> List.map ~f:(Point2d.of_string %> Option.get_exn_or "invalid point")

module Part_1 = struct
  let unparse rect = Fun.uncurry Point2d.area rect |> Int.to_string

  let go points =
    Iter.diagonal_l points |> AreaHeap.of_iter |> AreaHeap.take
    |> Option.get_exn_or "no points"
    |> snd

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* Problem description update *)

(* Example run-through, again *)

module Part_2 = struct
  let parse = Fun.id
  let unparse = Fun.id
  let go = Fun.id

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
