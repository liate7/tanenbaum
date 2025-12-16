open! Common

let year = 2025
let day = 8

let example =
  "162,817,812\n\
   57,618,57\n\
   906,360,560\n\
   592,479,940\n\
   352,342,300\n\
   466,668,158\n\
   542,29,236\n\
   431,825,988\n\
   739,650,466\n\
   52,470,668\n\
   216,146,977\n\
   819,987,18\n\
   117,168,530\n\
   805,96,715\n\
   346,949,466\n\
   970,615,88\n\
   941,993,340\n\
   862,61,35\n\
   984,92,344\n\
   425,690,689\n"

(* Their plan is to connect the junction boxes with long strings of lights. Most of the
   junction boxes don't provide electricity; however, when two junction boxes are connected by
   a string of lights, electricity can pass between those two junction boxes.

   The Elves are trying to figure out which junction boxes to connect so that electricity can
   reach every junction box. They even have a list of all of the junction boxes' positions in
   3D space (your puzzle input).

   This list describes the position of 20 junction boxes, one per line. Each position is given
   as X,Y,Z coordinates.

   To save on string lights, the Elves would like to focus on connecting pairs of junction
   boxes that are as close together as possible according to straight-line distance.

   Your list contains many junction boxes; connect together the 1000 pairs of junction boxes
   which are closest together. Afterward, what do you get if you multiply together the sizes of
   the three largest circuits? *)

(* So, the first junction box in the list is at X=162, Y=817, Z=812.

   In this example, the two junction boxes which are closest together are 162,817,812 and
   425,690,689.

   Now, the two junction boxes which are closest together but aren't already directly connected
   are 162,817,812 and 431,825,988. After connecting them, since 162,817,812 is already
   connected to another junction box, there is now a single circuit which contains three
   junction boxes and an additional 17 circuits which contain one junction box each.

   The next two junction boxes to connect are 906,360,560 and 805,96,715. After connecting
   them, there is a circuit containing 3 junction boxes, a circuit containing 2 junction boxes,
   and 15 circuits which contain one junction box each.

   The next two junction boxes are 431,825,988 and 425,690,689. Because these two junction
   boxes were already in the same circuit, nothing happens!

   After making the ten shortest connections, there are 11 circuits: one circuit which contains
   5 junction boxes, one circuit which contains 4 junction boxes, two circuits which contain 2
   junction boxes each, and seven circuits which each contain a single junction
   box. Multiplying together the sizes of the three largest circuits (5, 4, and one of the
   circuits of size 2) produces 40.*)

(* Planning:
   We need:
   - A union-find
     (actually, not quite!  We want the equivalence classes, and I /think/ U-F often
     avoids explicitly having those around.)
   - A way to get point pairs by minimum distance (probably a heap)
   - A union-find -> ~ set of connected components, ideally ordered by size *)

module Box = struct
  type t = { x : int; y : int; z : int }

  let of_string str =
    let[@warning "-8"] [ x; y; z ] = String.split_on_char ~by:',' str in
    {
      x = Int.of_string_exn x;
      y = Int.of_string_exn y;
      z = Int.of_string_exn z;
    }

  let compare =
    Fun.lexicographic (fun { x = l; _ } { x = r; _ } -> Int.compare l r)
    @@ Fun.lexicographic (fun { y = l; _ } { y = r; _ } -> Int.compare l r)
    @@ fun { z = l; _ } { z = r; _ } -> Int.compare l r

  let dist l r =
    let magn_on f l r = Float.(f r - f l) ** 2. in
    sqrt
    @@ Float.(
         magn_on (fun { x; _ } -> of_int x) l r
         + magn_on (fun { y; _ } -> of_int y) l r
         + magn_on (fun { z; _ } -> of_int z) l r)
end

module UF = struct
  type 'a t = { value : 'a; mutable info : 'a info; id : int }
  and 'a info = Parent of 'a t | Size of int

  let make_id =
    let n = ref 0 in
    fun () ->
      if !n = -1 then failwith "Ran out of ids"
      else (
        incr n;
        !n)

  let make value = { value; info = Size 1; id = make_id () }
  let value { value; _ } = value

  let rec find t =
    match t.info with
    | Size _ -> t
    | Parent p ->
        let p = find p in
        t.info <- Parent (find p);
        p

  let merge l r =
    let l, r = (find l, find r) in
    if not @@ Equal.physical l r then
      match (l.info, r.info) with
      | Size l', Size r' when l' > r' ->
          r.info <- Parent l;
          l.info <- Size (l' + r')
      | Size l', Size r' ->
          l.info <- Parent r;
          r.info <- Size (l' + r')
      | Parent _, _ | _, Parent _ -> assert false

  let equiv l r = Equal.physical (find l) (find r)
  let ( =~= ) = equiv
  let hash { id; _ } = Int.hash id
  let size t = match (find t).info with Size s -> s | _ -> assert false
end

module DistsHeap = Heap.Make_from_compare (struct
  type t = Box.t UF.t * Box.t UF.t

  let compare =
    let unboxed_dist (l, r) = Box.dist (UF.value l) (UF.value r) in
    let unboxed_compare l r = Box.compare (UF.value l) (UF.value r) in
    Fun.lexicographic (fun l r ->
        Float.compare (unboxed_dist l) (unboxed_dist r))
    @@ Pair.compare unboxed_compare unboxed_compare
end)

let parse =
  String.lines_iter %> Iter.map ~f:Box.of_string %> Iter.map ~f:UF.make
  %> Iter.to_list

let unparse = Int.to_string

module Part_1 = struct
  let go boxes =
    let heap = Iter.diagonal_l boxes |> DistsHeap.of_iter in
    Iter.(
      heap
      |> unfoldr (DistsHeap.take %> Option.map Pair.swap)
      |> take 1000
      |> iter ~f:(fun (l, r) -> UF.merge l r));
    Iter.(
      boxes |> of_list |> map ~f:UF.find
      |> group_by ~eq:Equal.physical ~hash:UF.hash
      |> map ~f:(fun l -> List.head_opt l |> Option.map_or ~default:0 UF.size)
      |> sort ~cmp:(fun l r -> Int.compare r l)
      |> take 3 |> reduce ~m:Monoid.mul)

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* The Elves were right; they definitely don't have enough extension cables. You'll need to
   keep connecting junction boxes together until they're all in one large circuit.

   The Elves need to know how far those junction boxes are from the wall so they can pick the
   right extension cable. Continue connecting the closest unconnected pairs of junction boxes
   together until they're all in the same circuit. What do you get if you multiply together the
   X coordinates of the last two junction boxes you need to connect? *)

(* Continuing the above example, the first connection which causes all of the junction boxes to
   form a single circuit is between the junction boxes at 216,146,977 and
   117,168,530. [M]ultiplying the X coordinates of those two junction boxes (216 and 117)
   produces 25272. *)

module Part_2 = struct
  let go boxes =
    let count = List.length boxes in
    let heap = Iter.diagonal_l boxes |> DistsHeap.of_iter in
    heap
    |> Iter.unfoldr (DistsHeap.take %> Option.map Pair.swap)
    |> Iter.find (fun (l, r) ->
           UF.merge l r;
           Option.return_if (UF.size l = count) (l, r))
    |> Option.get_exn_or "Never fully connected, somehow"
    |> fun (l, r) -> (UF.value l).x * (UF.value r).x

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
