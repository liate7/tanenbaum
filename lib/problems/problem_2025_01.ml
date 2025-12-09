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
  type t = Left of int | Right of int

  let of_string str =
    match String.get str 0 with
    | 'R' -> Right (Int.of_string_exn <| String.drop 1 str)
    | 'L' -> Left (Int.of_string_exn <| String.drop 1 str)
    | _ -> failwith "Not a valid rotation"
end

let start = 50
let ( mod ) = Int.rem

let parse s =
  String.lines s
  |> List.filter ~f:(Fun.negate @@ String.equal "")
  |> List.map ~f:Rotation.of_string

module Part_1 = struct
  let rot at = function
    | Rotation.Left n -> (at - n) mod 100
    | Rotation.Right n -> (at + n) mod 100

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

let div_mod l r = (l / r, l mod r)

module Part_2 = struct
  let rot place at (rot : Rotation.t) =
    let un_mod = match rot with Left n -> at - n | Right n -> at + n in
    let quot, rem = div_mod un_mod 100 in
    place |> Ref.update (( + ) @@ Int.abs quot) |> ignore;
    rem

  let go rs =
    let passes = ref 0 in
    rs |> List.fold_left ~f:(rot passes) ~init:start |> ignore;
    !passes

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> Int.to_string
end
