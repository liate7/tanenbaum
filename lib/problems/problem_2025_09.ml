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

module Rect = struct
  open Point2d

  type t = Point2d.t * Point2d.t

  let of_pts (l, r) = Option.return_if (l.x <> r.x && l.y <> r.y) (l, r)
  let area (l, r) = (abs (r.x - l.x) + 1) * (abs (r.y - l.y) + 1)

  let contains (l, r) { x; y } =
    Range.(contains (l.x -- r.x) x && contains (l.y -- r.y) y)

  let perimeter (l, r) =
    let l = map2 min l r and r = map2 max l r in
    Iter.(
      append_l
        [
          l.x -- r.x |> map ~f:(fun x -> { x; y = l.y });
          l.x -- r.x |> map ~f:(fun x -> { x; y = r.y });
          l.y -- r.y |> map ~f:(fun y -> { x = l.x; y });
          l.y -- r.y |> map ~f:(fun y -> { x = r.x; y });
        ])

  let points_in (l, r) =
    let rec go l r : Point2d.t Iter.t =
      if l.y > r.y then Iter.empty
      else
        Iter.append
          Iter.(l.x -- r.x |> map ~f:(fun x -> { x; y = l.y }))
          (go (map_y inc l) r)
    in
    let l = map2 min l r and r = map2 max l r in
    go l r
end

module AreaHeap = Heap.Make_from_compare (struct
  type t = Rect.t

  let compare =
    let rev_dist_compare l r = Int.compare (Rect.area r) (Rect.area l) in
    Fun.lexicographic rev_dist_compare
    @@ Pair.compare Point2d.compare Point2d.compare
end)

let parse str =
  str |> String.lines
  |> List.map ~f:(Point2d.of_string %> Option.get_exn_or "invalid point")

let unparse rect = Rect.area rect |> Int.to_string

module Part_1 = struct
  let go points =
    Iter.diagonal_l points |> AreaHeap.of_iter |> AreaHeap.take
    |> Option.get_exn_or "no points"
    |> snd

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* The Elves just remembered: they can only switch out tiles that are red or green. So, your
   rectangle can only include red or green tiles.

   In your list, every red tile is connected to the red tile before and after it by a straight
   line of green tiles. The list wraps, so the first red tile is also connected to the last red
   tile. Tiles that are adjacent in your list will always be on either the same row or the same
   column.

   Using two red tiles as opposite corners, what is the largest area of any rectangle you can
   make using only red and green tiles?*)

(* Using the same example as before, the tiles marked X would be green:

   ..............
   .......#XXX#..
   .......X...X..
   ..#XXXX#...X..
   ..X........X..
   ..#XXXXXX#.X..
   .........X.X..
   .........#X#..
   ..............

   In addition, all of the tiles inside this loop of red and green tiles are also green. So, in
   this example, these are the green tiles:

   ..............
   .......#XXX#..
   .......XXXXX..
   ..#XXXX#XXXX..
   ..XXXXXXXXXX..
   ..#XXXXXX#XX..
   .........XXX..
   .........#X#..
   ..............

   The remaining tiles are never red nor green.

   The largest rectangle you can make in this example using only red and green tiles has area
   24. One way to do this is between 9,5 and 2,3:

   ..............
   .......#XXX#..
   .......XXXXX..
   ..OOOOOOOOXX..
   ..OOOOOOOOXX..
   ..OOOOOOOOXX..
   .........XXX..
   .........#X#..
   .............. *)

(* Planning:
   need:
   - polygon from bounding points
   - check rect in polygon
     - check all 4 points in the polygon
*)

(* After failing on my own, looked on the subreddit and found coordinate compression; trying
   that *)

let pairs it f =
  let acc = ref None in
  it (fun y ->
      (match !acc with None -> () | Some x -> f (x, y));
      acc := Some y)

module Polygon : sig
  type t

  val of_bounding_points : Point2d.t list -> t
  val contains : t -> Point2d.t * Point2d.t -> bool
end = struct
  let compressor pts =
    let open Point2d in
    let xs, ys = pts |> List.map ~f:(map_to_pair Fun.id) |> List.split in
    let module IntSet = Set.Make (Int) in
    let module IntMap = Map.Make (Int) in
    let compressor ls =
      let set = IntSet.of_list ls in
      let map =
        set |> IntSet.to_iter |> Iter.zip_i
        |> Iter.map ~f:(fun (l, r) -> (r, l))
        |> IntMap.of_iter
      in
      fun idx -> IntMap.get idx map |> Option.get_exn_or "???"
    in
    let compress_x = compressor xs and compress_y = compressor ys in
    fun { x; y } -> { x = compress_x x; y = compress_y y }

  type edge =
    | Horiz of { y : int; bounds : Range.t }
    | Vert of { x : int; bounds : Range.t }

  module PointMap = Map.Make (Point2d)

  type t = {
    contains : bool Lazy.t PointMap.t;
    compress : Point2d.t -> Point2d.t;
  }

  let hit_edge (pt : Point2d.t) edge =
    match edge with
    | Horiz { y; bounds } -> y = pt.y && Range.contains bounds pt.x
    | Vert { x; bounds } -> x = pt.x && Range.contains bounds pt.y

  let cast dir =
    Iter.iterate
      (match dir with
      | `Up -> Point2d.map_y sub
      | `Down -> Point2d.map_y inc
      | `Left -> Point2d.map_x sub
      | `Right -> Point2d.map_x inc)

  let contains_pt (edges, bounds) pt =
    let ray_hits dir =
      cast dir pt
      |> Iter.take_while ~f:(Rect.contains bounds)
      |> Iter.exists ~f:(fun pt -> List.exists ~f:(hit_edge pt) edges)
    in
    Rect.contains bounds pt
    && List.for_all ~f:ray_hits [ `Up; `Down; `Left; `Right ]

  let of_bounding_points pts =
    let compress = compressor pts in
    let pts = List.map ~f:compress pts in
    let hd = List.hd pts in
    let edges =
      let it = Iter.append (Iter.of_list pts) (Iter.singleton hd) in
      it |> pairs
      |> Iter.map ~f:(fun ((l, r) : Point2d.t * Point2d.t) ->
             if l.x = r.x then
               Vert { x = l.x; bounds = Range.(min l.y r.y -- max l.y r.y) }
             else if l.y = r.y then
               Horiz { y = l.y; bounds = Range.(min l.x r.x -- max l.x r.x) }
             else failwith "Invalid pair")
      |> Iter.to_list
    in
    let bounds =
      pts
      |> List.fold_left ~init:(hd, hd) ~f:(fun (l, r) pt ->
             (Point2d.map2 min l pt, Point2d.map2 max r pt))
    in
    let contains =
      Iter.(
        Rect.points_in bounds
        |> map ~f:(fun pt -> (pt, lazy (contains_pt (edges, bounds) pt)))
        |> PointMap.of_iter)
    in
    { contains; compress }

  let contains { contains; compress } rect =
    let rect = Pair.map_same compress rect in
    Rect.perimeter rect
    |> Iter.for_all ~f:(fun pt ->
           PointMap.get pt contains |> Option.map_or ~default:false Lazy.force)
end

module Part_2 = struct
  let parse str =
    let pts = parse str in
    (pts, Polygon.of_bounding_points pts)

  let go (pts, poly) =
    Iter.diagonal_l pts
    |> Iter.filter_map ~f:Rect.of_pts
    |> AreaHeap.of_iter
    |> Iter.unfoldr (AreaHeap.take %> Option.map (fun (x, y) -> (y, x)))
    |> Iter.filter ~f:(Polygon.contains poly)
    |> Iter.head
    |> Option.get_exn_or "no points"

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
