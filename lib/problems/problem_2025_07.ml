open! Common

let year = 2025
let day = 7

let example =
  ".......S.......\n\
   ...............\n\
   .......^.......\n\
   ...............\n\
   ......^.^......\n\
   ...............\n\
   .....^.^.^.....\n\
   ...............\n\
   ....^.^...^....\n\
   ...............\n\
   ...^.^...^.^...\n\
   ...............\n\
   ..^...^.....^..\n\
   ...............\n\
   .^.^.^.^.^...^.\n\
   ...............\n"

(* You quickly locate a diagram of the tachyon manifold (your puzzle input). A tachyon beam
   enters the manifold at the location marked S; tachyon beams always move downward. Tachyon
   beams pass freely through empty space (.). However, if a tachyon beam encounters a splitter
   (^), the beam is stopped; instead, a new tachyon beam continues from the immediate left and
   from the immediate right of the splitter.

   To repair the teleporter, you first need to understand the beam-splitting properties of the
   tachyon manifold.

   Analyze your manifold diagram. How many times will the beam be split? *)

(* In this example, the incoming tachyon beam (|) extends downward from S until it reaches the
   first splitter:

   .......S.......
   .......|.......
   .......^.......
   ...............
   ......^.^......
   ...............
   .....^.^.^.....
   ...............
   ....^.^...^....
   ...............
   ...^.^...^.^...
   ...............
   ..^...^.....^..
   ...............
   .^.^.^.^.^...^.
   ...............

   At that point, the original beam stops, and two new beams are emitted from the splitter:

   .......S.......
   .......|.......
   ......|^|......
   ...............
   ......^.^......
   ...............
   .....^.^.^.....
   ...............
   ....^.^...^....
   ...............
   ...^.^...^.^...
   ...............
   ..^...^.....^..
   ...............
   .^.^.^.^.^...^.
   ...............

   Those beams continue downward until they reach more splitters:

   .......S.......
   .......|.......
   ......|^|......
   ......|.|......
   ......^.^......
   ...............
   .....^.^.^.....
   ...............
   ....^.^...^....
   ...............
   ...^.^...^.^...
   ...............
   ..^...^.....^..
   ...............
   .^.^.^.^.^...^.
   ...............

   At this point, the two splitters create a total of only three tachyon beams, since they are
   both dumping tachyons into the same place between them:

   .......S.......
   .......|.......
   ......|^|......
   ......|.|......
   .....|^|^|.....
   ...............
   .....^.^.^.....
   ...............
   ....^.^...^....
   ...............
   ...^.^...^.^...
   ...............
   ..^...^.....^..
   ...............
   .^.^.^.^.^...^.
   ...............

   This process continues until all of the tachyon beams reach a splitter or exit the manifold:

   .......S.......
   .......|.......
   ......|^|......
   ......|.|......
   .....|^|^|.....
   .....|.|.|.....
   ....|^|^|^|....
   ....|.|.|.|....
   ...|^|^|||^|...
   ...|.|.|||.|...
   ..|^|^|||^|^|..
   ..|.|.|||.|.|..
   .|^|||^||.||^|.
   .|.|||.||.||.|.
   |^|^|^|^|^|||^|
   |.|.|.|.|.|||.|

   In this example, a tachyon beam is split a total of 21 times. *)

module type Setoid_entry = sig
  type t
  type elt

  val map_elt : (elt -> elt) -> t -> t
  val elt : t -> elt
end

module type Setoid = sig
  type t
  type elt

  val of_list : elt list -> t
  val singleton : elt -> t
  val intersect : t -> t -> t
  val diff : t -> t -> t
  val cardinal : t -> int

  module Entry : Setoid_entry with type elt := elt

  val add_iter : t -> Entry.t Iter.t -> t
  val to_iter : t -> Entry.t Iter.t

  (* Visualization *)

  val min_elt : t -> elt
  val max_elt : t -> elt
end

module type ARG = sig
  type elt = Int.t

  module Entry : sig
    include Setoid_entry with type elt := elt

    val to_char : t -> char
  end

  include Setoid with type elt := Int.t with module Entry := Entry
end

module Tachyon_manifold (S : ARG) = struct
  type t = { start : int; splits : S.t list }

  let of_string str =
    let start, lines =
      str |> String.lines
      |> List.drop_while ~f:(fun l -> not @@ String.contains l 'S')
      |> List.hd_tl
    in
    let start = String.find ~sub:"S" start in
    let splits =
      Iter.(
        of_list lines
        |> filter ~f:(Fun.flip String.contains '^')
        |> map ~f:(String.find_all_l ~sub:"^")
        |> map ~f:S.of_list |> to_list)
    in
    { start; splits }

  let tick (count, beams) splits =
    let to_split = S.intersect beams splits
    and continue = S.diff beams splits in
    ( count + S.cardinal to_split,
      Iter.(
        to_split |> S.to_iter
        |> flat_map_l ~f:(fun x ->
               [ S.Entry.map_elt sub x; S.Entry.map_elt inc x ])
        |> S.add_iter continue) )

  let run { start; splits } =
    splits |> List.fold_left ~init:(0, S.singleton start) ~f:tick

  let splits t = run t |> fst
  let paths t = run t |> snd |> S.cardinal

  let visualize t =
    let _, beams = run t in
    let min = S.min_elt beams and max = S.max_elt beams in
    let new_line = String.make (1 + max - min) '.' in
    let idx entry = S.Entry.elt entry - min in
    let format_line ?(init = new_line) entry_char s =
      Iter.(
        s |> S.to_iter
        |> fold ~f:(fun str entry ->
               String.set str (idx entry) (entry_char entry)))
        ~init
    in
    let f acc splits =
      let count, beams = tick acc splits in
      let beams_str = format_line S.Entry.to_char beams in
      Printf.printf "%s\n" @@ format_line ~init:beams_str (Fun.const '^') splits;
      Printf.printf "%s\n" beams_str;
      (count, beams)
    in
    Printf.printf "%s\n" @@ format_line (Fun.const 'S') @@ S.singleton t.start;
    Printf.printf "%s\n" @@ format_line S.Entry.to_char @@ S.singleton t.start;
    t.splits |> List.fold_left ~f ~init:(0, S.singleton t.start)
end

module Part_1 = struct
  module IntSet : sig
    include ARG with type elt = Int.t with type Entry.t = Int.t

    val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  end = struct
    include Set.Make (Int)

    let intersect = inter

    module Entry = struct
      type t = Int.t

      let map_elt f x = f x
      let elt = Fun.id
      let to_char = Fun.const '|'
    end

    let pp_hum f t = Format.fprintf f "%s" @@ to_string Int.to_string t
  end

  module Classical_manifold = Tachyon_manifold (IntSet)

  let parse = Classical_manifold.of_string
  let unparse = Int.to_string
  let go = Classical_manifold.splits

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* With a quantum tachyon manifold, only a single tachyon particle is sent through the
   manifold. A tachyon particle takes both the left and right path of each splitter
   encountered.

   Since this is impossible, the manual recommends the many-worlds interpretation of quantum
   tachyon splitting: each time a particle reaches a splitter, it's actually time itself which
   splits. In one timeline, the particle went left, and in the other timeline, the particle
   went right.

   To fix the manifold, what you really need to know is the number of timelines active after a
   single particle completes all of its possible journeys through the manifold. *)

(* In the above example, there are many timelines. For instance, there's the timeline where the
   particle always went left:

   .......S.......
   .......|.......
   ......|^.......
   ......|........
   .....|^.^......
   .....|.........
   ....|^.^.^.....
   ....|..........
   ...|^.^...^....
   ...|...........
   ..|^.^...^.^...
   ..|............
   .|^...^.....^..
   .|.............
   |^.^.^.^.^...^.
   |..............

   Or, there's the timeline where the particle alternated going left and right at each
   splitter:

   .......S.......
   .......|.......
   ......|^.......
   ......|........
   ......^|^......
   .......|.......
   .....^|^.^.....
   ......|........
   ....^.^|..^....
   .......|.......
   ...^.^.|.^.^...
   .......|.......
   ..^...^|....^..
   .......|.......
   .^.^.^|^.^...^.
   ......|........

   Or, there's the timeline where the particle ends up at the same point as the alternating
   timeline, but takes a totally different path to get there:

   .......S.......
   .......|.......
   ......|^.......
   ......|........
   .....|^.^......
   .....|.........
   ....|^.^.^.....
   ....|..........
   ....^|^...^....
   .....|.........
   ...^.^|..^.^...
   ......|........
   ..^..|^.....^..
   .....|.........
   .^.^.^|^.^...^.
   ......|........

   In this example, in total, the particle ends up on 40 different timelines.

   Apply the many-worlds interpretation of quantum tachyon splitting to your manifold
   diagram. In total, how many different timelines would a single tachyon particle end up
   on? *)

module Bag (O : Map.OrderedType) :
  Setoid with type elt = O.t with type Entry.t = O.t * Int.t = struct
  module Map' = Map.Make (O)

  type t = int Map'.t
  type elt = O.t

  let of_list l =
    Iter.(l |> of_list |> map ~f:(fun x -> (x, 1)) |> Map'.of_iter)

  let singleton = Fun.flip Map'.singleton 1

  let intersect : t -> t -> t =
    Map'.merge_safe ~f:(fun _ -> function
      | `Both (l, r) -> Option.some @@ max l r
      | _ -> None)

  let diff : t -> t -> t =
    Map'.merge_safe ~f:(fun _ -> function
      | `Left l -> Some l
      | `Both _ | `Right _ -> None)

  let cardinal : t -> int =
    Map'.to_iter %> Iter.map ~f:snd %> Iter.reduce ~m:Monoid.add

  module Entry = struct
    type t = elt * Int.t

    let map_elt f (x, count) = (f x, count)
    let elt = fst
  end

  let to_iter : t -> Entry.t Iter.t = Map'.to_iter
  let add_iter = Map'.add_iter_with ~f:(fun _ -> ( + ))
  let min_elt = Map'.min_binding %> fst
  let max_elt = Map'.max_binding %> fst
end

module Part_2 = struct
  module IntBag : sig
    include ARG with type elt = Int.t with type Entry.t = Int.t * Int.t

    val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  end = struct
    include Bag (Int)

    module Entry = struct
      include Entry

      let to_char (_, count) =
        try String.get ".123456789" count with Invalid_argument _ -> '!'
    end

    let pp_hum f t =
      Format.fprintf f "{@[%a@]}"
        ( List.pp @@ fun f (l, r) ->
          Format.fprintf f "@[%a:%a@]" Int.pp l Int.pp r )
      @@ (to_iter t |> Iter.to_list)
  end

  module Quantum_manifold = Tachyon_manifold (IntBag)

  let parse = Quantum_manifold.of_string
  let unparse = Int.to_string
  let go = Quantum_manifold.paths

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
