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

  (* Part 2 *)
  let decompose = function `L n -> (`L, n) | `R n -> (`R, n)

  let to_fun = function
    | `R -> ( + )
    | `L ->
        (* This could just be `( - )`, but this makes the using code cleaner
         and is logically just as correct *)
        fun l r -> (match l with 0 -> 100 | l -> l) - r
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

let div_mod l r = (Int.floor_div l r, l mod r)
let ( /% ) = div_mod

module Part_2 = struct
  type state = { zeros : int; pos : int }

  (* Based heavily on [[https://entropicthoughts.com/advent-of-code-in-dialog]],
     mod (some?) dialog weirdness *)
  let rot { zeros; pos = at } action =
    (* (This could just be a different parser, but I otherwise like the pairing) *)
    let dir, n = Rotation.decompose action in
    (* Always hit zero at least `div` times, so decompose that out *)
    let zeros, change = n /% 100 |> Pair.map_fst (( + ) zeros) in
    let pos = Rotation.to_fun dir at change in
    if pos = pos mod 100 && pos <> 0 then
      (* We didn't pass 0 again; we're good *)
      { zeros; pos }
    else
      (* We passed 0 again, necessarily once; fix it up *)
      { zeros = zeros + 1; pos = pos mod 100 }

  let go rs =
    (rs |> List.fold_left ~f:rot ~init:{ zeros = 0; pos = start }).zeros

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
