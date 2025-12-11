open! Common

let year = 2025
let day = 4

let example =
  "..@@.@@@@.\n\
   @@@.@.@.@@\n\
   @@@@@.@.@@\n\
   @.@@@@..@.\n\
   @@.@@@@.@@\n\
   .@@@@@@@.@\n\
   .@.@.@.@@@\n\
   @.@@@.@@@@\n\
   .@@@@@@@@.\n\
   @.@.@@@.@.\n"

(* If you can optimize the work the forklifts are doing, maybe they would have time to spare to
   break through the wall.

   The rolls of paper (@) are arranged on a large grid; the Elves even have a helpful diagram
   (your puzzle input) indicating where everything is located.

   The forklifts can only access a roll of paper if there are fewer than four rolls of paper in
   the eight adjacent positions. *)

(* In this example, there are 13 rolls of paper that can be accessed by a forklift
   (marked with x):

   ..xx.xx@x.
   x@@.@.@.@@
   @@@@@.x.@@
   @.@@@@..@.
   x@.@@@@.@x
   .@@@@@@@.@
   .@.@.@.@@@
   x.@@@.@@@@
   .@@@@@@@@.
   x.x.@@@.x. *)

module Grid = struct
  type t = { dim : int * int; data : [ `Roll | `Empty ] Array.t Array.t }

  let of_string str =
    let f =
      Fun.(
        String.to_array
        %> Array.map ~f:(function
             | '@' -> `Roll
             | '.' -> `Empty
             | c -> failwith @@ Printf.sprintf "Not a valid character: '%c'" c))
    in
    let data =
      str |> String.trim |> String.lines_iter |> Iter.map ~f |> Iter.to_array
    in
    { data; dim = (Array.length @@ Array.get data 0, Array.length data) }

  let to_string { data; dim = _ } =
    Iter.(
      of_array data
      |> map ~f:(fun arr ->
             Array.map ~f:(function `Roll -> '@' | `Empty -> '.') arr
             |> String.of_array)
      |> String.concat_iter ~sep:"\n")

  let in_bounds { dim = width, height; data = _ } (x, y) =
    0 <= x && x < width && 0 <= y && y < height

  let get (x, y) ({ data; dim = _ } as t) =
    if in_bounds t (x, y) then Some data.(x).(y) else None

  let ( .%() ) t at = get at t |> Option.get_exn_or "Invalid index"

  module Map' = Map.Make (IntIntOrder)

  let map_monoid =
    Monoid.
      { identity = Map'.empty; reduce = Map'.union (fun _ l r -> Some (l + r)) }

  let neighbors (x, y) =
    let id = Fun.id and incr x = x + 1 and decr x = x - 1 in
    [
      (id, incr);
      (id, decr);
      (incr, incr);
      (incr, id);
      (incr, decr);
      (decr, incr);
      (decr, id);
      (decr, decr);
    ]
    |> List.map ~f:(fun (f, g) -> (f x, g y))

  let is_roll at t = match t.%(at) with `Roll -> true | `Empty -> false

  let adjacencies t at =
    Iter.(
      neighbors at |> of_list
      |> filter ~f:(in_bounds t)
      |> filter ~f:(Fun.flip is_roll t)
      |> length |> Map'.singleton at)

  let adjacency ({ dim = width, height; data = _ } as t) =
    Iter.(
      product (0 --^ width) (0 --^ height)
      |> filter ~f:(fun at -> is_roll at t)
      |> map ~f:(adjacencies t)
      |> reduce ~m:map_monoid)
end

let parse = Grid.of_string
let unparse = Int.to_string

let mark_taken grid =
  let to_mark =
    grid.Grid.data
    |> Array.map ~f:(Array.map ~f:(function `Roll -> '@' | `Empty -> '.'))
  in
  grid |> Grid.adjacency
  |> Grid.Map'.filter (fun _ adj -> adj < 4)
  |> Grid.Map'.keys
  |> Iter.iter ~f:(fun (y, x) ->
         if Char.(to_mark.(y).(x) = 'x') then
           Printf.eprintf "warning: Re-marking (%d, %d)" y x
         else to_mark.(y).(x) <- 'x');
  Iter.(
    of_array to_mark |> map ~f:String.of_array |> String.concat_iter ~sep:"\n")

module Part_1 = struct
  let go grid =
    Iter.(
      grid |> Grid.adjacency |> Grid.Map'.to_iter
      |> filter ~f:(fun (_, adj) -> adj < 4)
      |> length)

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
