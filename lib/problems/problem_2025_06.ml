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
    | None -> List.rev acc
    | Some (l, ls) -> go (List.rev l :: acc) ls
  in
  go [] ls

module Part_1 = struct
  let tokenize str =
    Iter.(
      str |> String.trim |> String.lines_iter
      |> map ~f:(Re.split blanks)
      |> to_list)

  let parse str =
    let f = function
      | [] -> assert false
      | "+" :: nums -> (Monoid.add, List.map ~f:Int.of_string_exn nums)
      | "*" :: nums -> (Monoid.mul, List.map ~f:Int.of_string_exn nums)
      | _ -> assert false
    in
    tokenize str |> transpose |> List.map ~f

  let unparse = Int.to_string

  let go =
    List.monoid_map_reduce
      ~f:(fun (m, nums) -> List.monoid_reduce ~m nums)
      ~m:Monoid.add

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* Cephalopod math is written right-to-left in columns. Each number is given in its own column,
   with the most significant digit at the top and the least significant digit at the
   bottom. (Problems are still separated with a column consisting only of spaces, and the
   symbol at the bottom of the problem is still the operator to use.) *)

(* Reading the problems right-to-left one column at a time, the problems are now quite
   different:

   - The rightmost problem is 4 + 431 + 623 = 1058
   - The second problem from the right is 175 * 581 * 32 = 3253600
   - The third problem from the right is 8 + 248 + 369 = 625
   - Finally, the leftmost problem is 356 * 24 * 1 = 8544

   Now, the grand total is 1058 + 3253600 + 625 + 8544 = 3263827. *)

(* So we need to turn
     1 2 3
     4 5 6
     7 8 9
   to
     7 4 1
     8 5 2
     9 6 3
   and
     1 2
     3 4
     5 6 7
   to
     5 3 1
     6 4 2
     7
*)

let split ~by:split_by iter f =
  let ret = ref None in
  let go x =
    if split_by x then (
      Option.iter Fun.(List.rev %> f) !ret;
      ret := None)
    else
      ret
      |> Ref.update
         @@ Option.map_or ~default:(Some [ x ])
         @@ Fun.(List.cons x %> Option.some)
  in
  iter go;
  Option.iter Fun.(List.rev %> f) !ret

module Part_2 = struct
  let parse str =
    let lines = str |> String.lines in
    let ( %> ) = Fun.( %> ) in
    let ops =
      List.last_opt lines
      |> Option.get_exn_or "input not empty"
      |> Re.split blanks
      |> List.map ~f:(function
           | "+" -> Monoid.add
           | "*" -> Monoid.mul
           | _ -> assert false)
    and nums =
      Iter.(
        List.take (List.length lines - 1) lines
        |> List.map ~f:String.to_list |> transpose |> of_list
        |> map ~f:String.(of_list %> ltrim %> rtrim %> rev)
        |> split ~by:String.is_empty
        |> map ~f:(List.map ~f:Int.of_string_exn)
        |> to_list)
    in
    List.combine ops nums

  let unparse = Int.to_string

  let go =
    List.monoid_map_reduce
      ~f:(fun (m, nums) -> List.monoid_reduce ~m nums)
      ~m:Monoid.add

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
