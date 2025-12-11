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
  type 'a t = {
    dim : int * int;
    data : ([> `Roll | `Empty ] as 'a) Array.t Array.t;
  }

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
    { data; dim = (Array.length data, Array.length @@ Array.get data 0) }

  let to_string { data; dim = _ } =
    Iter.(
      of_array data
      |> map ~f:(fun arr ->
             Array.map ~f:(function `Roll -> '@' | `Empty -> '.') arr
             |> String.of_array)
      |> String.concat_iter ~sep:"\n")

  let in_bounds { dim = height, width; data = _ } (y, x) =
    0 <= x && x < width && 0 <= y && y < height

  let get (y, x) ({ data; dim = _ } as t) =
    if in_bounds t (y, x) then Some data.(y).(x) else None

  let ( .%() ) t at = get at t |> Option.get_exn_or "Invalid index"

  module Map' = Map.Make (IntIntOrder)

  let map_monoid =
    Monoid.
      { identity = Map'.empty; reduce = Map'.union (fun _ l r -> Some (l + r)) }

  let neighbors (y, x) =
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
    |> List.map ~f:(fun (f, g) -> (f y, g x))

  let is_roll at t = match t.%(at) with `Roll -> true | `Empty -> false

  let adjacencies t at =
    Iter.(
      neighbors at |> of_list
      |> filter ~f:(in_bounds t)
      |> filter ~f:(Fun.flip is_roll t)
      |> length |> Pair.make at)

  let adjacency ({ dim = height, width; data = _ } as t) =
    Iter.(
      product (0 --^ height) (0 --^ width)
      |> filter ~f:(fun at -> is_roll at t)
      |> map ~f:(adjacencies t))

  let ( .%()<- ) { data; dim = _ } (y, x) value = data.(y).(x) <- value
  let copy t = { t with data = t.data |> Array.map ~f:Array.copy }

  let remove t at =
    let ret = copy t in
    ret.%(at) <- `Empty;
    ret
end

let parse = Grid.of_string
let unparse = Int.to_string

let mark_taken grid =
  let to_mark =
    grid.Grid.data
    |> Array.map ~f:(Array.map ~f:(function `Roll -> '@' | `Empty -> '.'))
  in
  grid |> Grid.adjacency |> Grid.Map'.of_iter
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
    grid |> Grid.adjacency |> Grid.Map'.of_iter
    |> Grid.Map'.filter (fun _ adj -> adj < 4)
    |> Grid.Map'.cardinal

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* Once a roll of paper can be accessed by a forklift, it can be removed. Once a roll of paper
   is removed, the forklifts might be able to access more rolls of paper, which they might also
   be able to remove. How many total rolls of paper could the Elves remove if they keep
   repeating this process? *)

(* Starting with the same example as above, here is one way you could remove as many rolls of
   paper as possible, using highlighted @ to indicate that a roll of paper is about to be
   removed, and using x to indicate that a roll of paper was just removed: *)

module Part_2 = struct
  let visualize grid =
    let rec go acc grid =
      let moveable =
        Iter.(
          grid |> Grid.adjacency
          |> filter ~f:(fun (_, adj) -> adj < 4)
          |> map ~f:fst |> to_list)
      in
      match moveable with
      | [] -> acc
      | moveable ->
          Printf.printf "Remove %d rolls of paper:\n%s\n\n"
            (List.length moveable)
          @@ mark_taken grid;
          let grid = List.fold_left ~f:Grid.remove ~init:grid moveable in
          go (acc + List.length moveable) grid
    in
    Printf.printf "Initial state:\n%s\n\n" @@ Grid.to_string grid;
    go 0 grid

  let go grid =
    let rec go acc grid =
      let moveable =
        Iter.(
          grid |> Grid.adjacency
          |> filter ~f:(fun (_, adj) -> adj < 4)
          |> map ~f:fst |> to_list)
      in
      match moveable with
      | [] -> acc
      | moveable ->
          let grid = List.fold_left ~f:Grid.remove ~init:grid moveable in
          go (acc + List.length moveable) grid
    in
    go 0 grid

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
