open! Common

let year = 2024
let day = 4

let example =
  "MMMSXXMASM\n\
   MSAMXMSMSA\n\
   AMXSXMAAMM\n\
   MSAMASMSMX\n\
   XMASAMXAMM\n\
   XXAMMXXAMA\n\
   SMSMSASXSS\n\
   SAXAMASAAA\n\
   MAMMMXMMMM\n\
   MXMXAXMASX\n"

let parse str =
  let ret = String.trim str |> String.lines |> List.map ~f:String.to_array in
  assert (
    (* All have the same length *)
    List.map ~f:Array.length ret
    |> List.sort_uniq ~cmp:Int.compare
    |> List.length = 1);
  Array.of_list ret

let directions =
  [ (-1, -1); (-1, 1); (1, -1); (1, 1); (-1, 0); (1, 0); (0, -1); (0, 1) ]

let step (f, g) (x, y) = (f + x, g + y)
let in_bounds ~bounds:(w, h) (x, y) = x >= 0 && x < w && y >= 0 && y < h

let neighbors ~bounds pt =
  List.map directions ~f:(fun dir -> (step dir pt, dir))
  |> List.filter ~f:(fun (pt, _) -> in_bounds ~bounds pt)

module Part_1 = struct
  let search ~bounds grid l pt dir =
    let rec go grid dir (x, y) = function
      | [] -> true
      | _ when not @@ in_bounds ~bounds (x, y) -> false
      | next :: rest when Char.(grid.(x).(y) = next) ->
          go grid dir (step dir (x, y)) rest
      | _ -> false
    in
    go grid dir pt l

  let run input =
    Result.guard_str @@ fun () ->
    let grid = parse input in
    let bounds = (Array.length grid, Array.length grid.(0)) in
    Array.foldi grid ~init:0 ~f:(fun init x row ->
        Array.foldi row ~init ~f:(fun acc y -> function
          | 'X' ->
              (neighbors ~bounds (x, y)
              |> List.count ~f:(fun (pt, dir) ->
                     search ~bounds grid [ 'M'; 'A'; 'S' ] pt dir))
              + acc
          | _ -> acc))
    |> Int.to_string
end

let corners ~bounds pt =
  List.map
    [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]
    ~f:(fun dir -> (step dir pt, dir))
  |> List.filter ~f:(fun (pt, _) -> in_bounds ~bounds pt)

let flip (f, g) (x, y) = (x - (2 * f), y - (2 * g))

module Part_2 = struct
  let check ~bounds grid (x, y) dir =
    Char.(grid.(x).(y) = 'M')
    &&
    let x, y = flip dir (x, y) in
    in_bounds ~bounds (x, y) && Char.(grid.(x).(y) = 'S')

  let run input =
    Result.guard_str @@ fun () ->
    let grid = parse input in
    let bounds = (Array.length grid, Array.length grid.(0)) in
    Array.foldi grid ~init:0 ~f:(fun init x row ->
        Array.foldi row ~init ~f:(fun acc y -> function
          | 'A' ->
              if
                corners ~bounds (x, y)
                |> List.count ~f:(fun (pt, dir) -> check ~bounds grid pt dir)
                = 2
              then succ acc
              else acc
          | _ -> acc))
    |> Int.to_string
end
