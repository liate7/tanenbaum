open! Common

(*
Maybe the lists are only off by a small amount! To find out, pair up the numbers
and measure how far apart they are. Pair up the smallest number in the left list
with the smallest number in the right list, then the second-smallest left number
with the second-smallest right number, and so on.

Within each pair, figure out how far apart the two numbers are; you'll need to
add up all of those distances. For example, if you pair up a 3 from the left list
with a 7 from the right list, the distance apart is 4; if you pair up a 9 with a
3, the distance apart is 6. *)

let year = 2024
let day = 1
let example = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3\n"

let pairs l =
  l
  |> List.filter_map ~f:(function
       | "" -> None
       | line ->
           Some
             (let[@warning "-8"] [ l; r ] = String.split ~by:"   " line in
              (Int.of_string_exn l, Int.of_string_exn r)))
  |> List.split

module Part_1 = struct
  let go (l, r) =
    List.sort ~cmp:Int.compare l
    |> List.map2 ~f:(fun l r -> Int.abs (l - r))
    <| List.sort ~cmp:Int.compare r
    |> List.fold_left ~f:( + ) ~init:0

  let run input : (string, string) result =
    Result.guard_str @@ fun () ->
    String.lines input |> pairs |> go |> Int.to_string
end

(*
The Historians can't agree on which group made the mistakes or how to read most
of the Chief's handwriting, but in the commotion you notice an interesting detail:
a lot of location IDs appear in both lists! Maybe the other numbers aren't location
IDs at all but rather misinterpreted handwriting.

This time, you'll need to figure out exactly how often each number from the left
list appears in the right list. Calculate a total similarity score by adding up
each number in the left list after multiplying it by the number of times that
number appears in the right list.*)

module Part_2 = struct
  let go (l, r) =
    let r' =
      List.fold_left ~init:IntMap.empty
        ~f:(fun acc x ->
          IntMap.update x
            (function None -> Some 1 | Some n -> Some (succ n))
            acc)
        r
    in
    List.to_seq l
    |> Seq.map (fun i -> i * IntMap.get_or i r' ~default:0)
    |> Seq.fold_left ( + ) 0

  let run input =
    Result.guard_str @@ fun () ->
    String.lines input |> pairs |> go |> Int.to_string
end
