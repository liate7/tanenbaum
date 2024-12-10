open! Common

let year = 2024
let day = 5

let example =
  "47|53\n\
   97|13\n\
   97|61\n\
   97|47\n\
   75|29\n\
   61|13\n\
   75|53\n\
   29|13\n\
   97|29\n\
   53|29\n\
   61|53\n\
   97|53\n\
   61|29\n\
   47|13\n\
   75|47\n\
   97|75\n\
   47|61\n\
   75|61\n\
   47|29\n\
   75|13\n\
   53|13\n\n\
   75,47,61,53,29\n\
   97,61,53,29,13\n\
   75,29,13\n\
   75,97,47,61,53\n\
   61,13,29\n\
   97,13,75,29,47"

module IntPairSet = Set.Make (struct
  type t = int * int

  let compare =
    Fun.lexicographic
      (fun (x, _) (x', _) -> Int.compare x x')
      (fun (_, y) (_, y') -> Int.compare y y')
end)

let parse str =
  match String.cut ~on:"\n\n" str with
  | None -> failwith @@ Printf.sprintf {|Can't parse "%s"|} str
  | Some (rules, updates) ->
      let rules =
        String.lines rules
        |> List.map ~f:(fun line ->
               let before, after =
                 String.cut line ~on:"|" |> Option.get_exn_or "Invalid rule"
               in
               (Int.of_string_exn before, Int.of_string_exn after))
        |> IntPairSet.of_list
      and updates =
        String.lines updates
        |> List.map
             ~f:Fun.(String.split ~by:"," %> List.map ~f:Int.of_string_exn)
      in
      (rules, updates)

let update_cmp rules l r =
  if IntPairSet.mem (r, l) rules then 1
  else if IntPairSet.mem (l, r) rules then -1
  else 0

let update_is_valid rules update = List.is_sorted ~cmp:(update_cmp rules) update

let middle_item lst =
  let len = List.length lst in
  List.nth lst @@ (len / 2)

module Part_1 = struct
  let run input =
    Result.guard_str @@ fun () ->
    let rules, updates = parse input in
    List.monoid_map_reduce ~m:Monoid.add updates ~f:(fun update ->
        if update_is_valid rules update then middle_item update else 0)
    |> Int.to_string
end

module Part_2 = struct
  let run input =
    Result.guard_str @@ fun () ->
    let rules, updates = parse input in
    let invalid_updates =
      List.filter ~f:(Fun.negate @@ update_is_valid rules) updates
    in
    let cmp = update_cmp rules in
    List.monoid_map_reduce ~m:Monoid.add invalid_updates ~f:(fun update ->
        List.sort ~cmp update |> middle_item)
    |> Int.to_string
end
