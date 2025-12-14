open! Common

let year = 2025
let day = 6

let example =
  "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  \n"

(* The math worksheet (your puzzle input) consists of a list of problems; each problem has a
   group of numbers that need to be either added ( + ) or multiplied ( * ) together.

   However, the problems are arranged a little strangely; they seem to be presented next to
   each other in a very long horizontal list.

   Each problem's numbers are arranged vertically; at the bottom of the problem is the symbol
   for the operation that needs to be performed. Problems are separated by a full column of
   only spaces. The left/right alignment of numbers within each problem can be ignored.

   given the grand total of adding together all of the answers to the individual problems. *)

(* So, this worksheet contains four problems:

    123 * 45 * 6 = 33210
    328 + 64 + 98 = 490
    51 * 387 * 215 = 4243455
    64 + 23 + 314 = 401

   In this worksheet, the grand total is 33210 + 490 + 4243455 + 401 = 4277556.*)

let blanks = Re.(compile @@ rep1 blank)

let transpose ls =
  let rec go acc ls =
    let f = function [] -> None | hd :: tl -> Some (hd, tl) in
    match
      ls
      |> Iter.(on_list @@ map ~f)
      |> Option.sequence_l |> Option.map List.split
    with
    | None -> acc
    | Some (l, ls) -> go (List.rev l :: acc) ls
  in
  go [] ls

let parse str =
  let f = function
    | [] -> assert false
    | "+" :: nums -> (Monoid.add, List.map ~f:Int.of_string_exn nums)
    | "*" :: nums -> (Monoid.mul, List.map ~f:Int.of_string_exn nums)
    | _ -> assert false
  in
  Iter.(
    str |> String.trim |> String.lines_iter
    |> map ~f:(Re.split blanks)
    |> to_list)
  |> transpose |> List.map ~f

let unparse = Int.to_string

module Part_1 = struct
  let go =
    List.monoid_map_reduce
      ~f:(fun (m, nums) -> List.monoid_reduce ~m nums)
      ~m:Monoid.add

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
