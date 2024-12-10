open! Common

let year = 2024
let day = 6

let example =
  "....#.....\n\
   .........#\n\
   ..........\n\
   ..#.......\n\
   .......#..\n\
   ..........\n\
   .#..^.....\n\
   ........#.\n\
   #.........\n\
   ......#...\n"

module IntIntSet = Set.Make (IntIntOrder)

module Grid = struct
  type t = { obstacles : IntIntSet.t; width : int; height : int }

  let in_bounds (x, y) t = x >= 0 && x < t.width && y >= 0 && y < t.height
  let has_obstacle pos t = IntIntSet.mem pos t.obstacles
end

let parse str =
  let lines = String.trim str |> String.lines
  and f init y str =
    String.foldi init str ~f:(fun (obstacles, start) x -> function
      | '#' -> (IntIntSet.add (x, y) obstacles, start)
      | '^' ->
          assert (Option.is_none start);
          (obstacles, Some (x, y))
      | _ -> (obstacles, start))
  in
  match lines |> List.foldi ~init:(IntIntSet.empty, None) ~f with
  | obstacles, Some start ->
      ( Grid.
          {
            obstacles;
            height = List.length lines;
            width = String.length @@ List.hd lines;
          },
        start )
  | _ -> failwith "No start position given"

module Dir = struct
  type t = Up | Down | Left | Right

  let to_delta = function
    | Up -> (0, -1)
    | Right -> (1, 0)
    | Down -> (0, 1)
    | Left -> (-1, 0)

  let rotate = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

  let step dir (x, y) =
    let f, g = to_delta dir in
    (f + x, g + y)

  let compare l r = IntIntOrder.compare (to_delta l) (to_delta r)
end

let positions_visited grid start =
  let visited = IntIntSet.empty |> IntIntSet.add start in
  let rec loop grid visited pos dir =
    let pos' = Dir.step dir pos in
    if not @@ Grid.in_bounds pos' grid then visited
    else if Grid.has_obstacle pos' grid then
      loop grid visited pos (Dir.rotate dir)
    else loop grid (IntIntSet.add pos' visited) pos' dir
  in
  loop grid visited start Dir.Up

module Part_1 = struct
  let run input =
    Result.guard_str @@ fun () ->
    parse input
    |> Fun.uncurry positions_visited
    |> IntIntSet.cardinal |> Int.to_string
end

module Part_2 = struct
  module IntIntMap = Map.Make (IntIntOrder)
  module DirSet = Set.Make (Dir)

  let has_loop start grid =
    let visited = IntIntMap.singleton start @@ DirSet.singleton Dir.Up in
    let rec loop grid visited pos dir =
      let pos' = Dir.step dir pos in
      if not @@ Grid.in_bounds pos' grid then false
      else if Grid.has_obstacle pos' grid then
        loop grid visited pos @@ Dir.rotate dir
      else
        let cur_directions =
          IntIntMap.get_or ~default:DirSet.empty pos' visited
        in
        if DirSet.mem dir cur_directions then true
        else
          let visited =
            IntIntMap.add pos' (DirSet.add dir cur_directions) visited
          in
          loop grid visited pos' dir
    in
    loop grid visited start Dir.Up

  let run input =
    Result.guard_str @@ fun () ->
    let grid, start = parse input in
    let visited = positions_visited grid start in
    IntIntSet.fold
      (fun (x, y) acc ->
        acc
        +
        if Grid.has_obstacle (x, y) grid then 0
        else if
          has_loop start
          @@ Grid.{ grid with obstacles = IntIntSet.add (x, y) grid.obstacles }
        then 1
        else 0)
      visited 0
    |> Int.to_string
end
