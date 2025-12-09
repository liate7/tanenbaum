open! Common

let year = 2025
let day = 1
let example = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n"

(*
The safe has a dial with only an arrow on it; around the dial are the numbers 0 through 99 in
order. As you turn the dial, it makes a small click noise as it reaches each number.

The attached document (your puzzle input) contains a sequence of rotations, one per line, which
tell you how to open the safe. A rotation starts with an L or R which indicates whether the
rotation should be to the left (toward lower numbers) or to the right (toward higher
numbers). Then, the rotation has a distance value which indicates how many clicks the dial
should be rotated in that direction.

The dial starts by pointing at 50.

The actual password is the number of times the dial is left pointing at 0 after any rotation in
the sequence.
 *)

module Rotation = struct
  type t = [ `L of int | `R of int ]

  let of_string str =
    match String.get str 0 with
    | 'R' -> `R (Int.of_string_exn <| String.drop 1 str)
    | 'L' -> `L (Int.of_string_exn <| String.drop 1 str)
    | _ -> failwith "Not a valid rotation"
end

let start = 50
let ( mod ) = Int.rem

let parse s =
  String.lines s
  |> List.filter ~f:(Fun.negate @@ String.equal "")
  |> List.map ~f:Rotation.of_string

module Part_1 = struct
  let rot at = function `L n -> (at - n) mod 100 | `R n -> (at + n) mod 100

  let go rs =
    rs
    |> List.scan_left ~f:rot ~init:start
    |> List.drop 1
    |> List.count ~f:(Int.equal 0)

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> Int.to_string
end

(*
you're actually supposed to count the number of times any click causes the dial to point at 0,
regardless of whether it happens during a rotation or at the end of one.
*)

let div_mod l r =
  let quot = if l > 0 then l / r else 1 + abs (l / r) in
  (quot, l mod r)

(* I actually ended up with ≈this; afterward is an attempt to do this analytically *)
module Part_2_brute_force = struct
  let expand = function
    | (`R 0 | `L 0) as zero -> [ zero ]
    | `R n -> List.repeat n [ `R 1 ]
    | `L n -> List.repeat n [ `L 1 ]

  let go rs = rs |> List.flat_map ~f:expand |> Part_1.go

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> Int.to_string
end

module Part_2_collapsed = struct
  let rot (state, at) (rot : Rotation.t) =
    let un_mod = match rot with `L n -> at - n | `R n -> at + n in
    let quot, rem = div_mod un_mod 100 in
    let quot = if at = 0 then max 0 @@ (quot - 1) else quot in
    (state + quot, rem)

  let go rs =
    let ret, _ = rs |> List.fold_left ~f:rot ~init:(0, start) in
    ret

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> Int.to_string

  (*
  Following the same rotations as in the above example, the dial points at zero a few extra
  times during its rotations:

    The dial starts by pointing at 50.
    The dial is rotated L68 to point at 82; during this rotation, it points at 0 once.
    The dial is rotated L30 to point at 52.
    The dial is rotated R48 to point at 0.
    The dial is rotated L5 to point at 95.
    The dial is rotated R60 to point at 55; during this rotation, it points at 0 once.
    The dial is rotated L55 to point at 0.
    The dial is rotated L1 to point at 99.
    The dial is rotated L99 to point at 0.
    The dial is rotated R14 to point at 14.
    The dial is rotated L82 to point at 32; during this rotation, it points at 0 once.

  In this example, the dial points at 0 three times at the end of a rotation, plus three more
  times during a rotation. So, in this example, the new password would be 6.
  *)
end

module Part_2 = Part_2_brute_force
