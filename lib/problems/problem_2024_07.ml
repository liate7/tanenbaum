open! Common

let year = 2024
let day = 7

let example =
  "190: 10 19\n\
   3267: 81 40 27\n\
   83: 17 5\n\
   156: 15 6\n\
   7290: 6 8 6 15\n\
   161011: 16 10 13\n\
   192: 17 8 14\n\
   21037: 9 7 18 13\n\
   292: 11 6 16 20\n"

module Test_f (Operation : sig
  type t

  val enum : t list
  val perform : t -> int -> int -> int
end) =
struct
  type t = int * int list

  let parse str =
    let lines = String.trim str |> String.lines
    and f line =
      let total, to_combine =
        String.cut ~on:": " line |> Option.get_exn_or "Invalid line"
      in
      ( Int.of_string_exn total,
        String.split ~by:" " to_combine |> List.map ~f:Int.of_string_exn )
    in
    List.map ~f lines

  let operations (total, to_combine) =
    let rec go acc = function
      | [] when acc = total -> Some []
      | [] -> None
      | i :: is ->
          List.fold_while ~init:None Operation.enum ~f:(fun _ op ->
              let next = Operation.perform op acc i in
              if next > total then (None, `Continue)
              else
                match go next is with
                | Some l -> (Some (op :: l), `Stop)
                | _ -> (None, `Continue))
    in
    match to_combine with i :: is -> go i is | [] -> go 0 []

  let is_valid t = Option.is_some @@ operations t
end

module Part_1 = struct
  module Operation = struct
    type t = Times | Plus

    let enum = [ Times; Plus ]
    let perform = function Times -> ( * ) | Plus -> ( + )
  end

  module Test = Test_f (Operation)

  let run (input : string) : (string, string) result =
    Result.guard_str_trace @@ fun () ->
    Test.parse input
    |> List.fold_left ~init:0 ~f:(fun sum ((total, _) as t) ->
           if Test.is_valid t then sum + total else sum)
    |> Int.to_string
end

module Part_2 = struct
  module Operation = struct
    type t = Times | Plus | Dec_concat

    let enum = [ Times; Dec_concat; Plus ]

    let perform = function
      | Times -> ( * )
      | Plus -> ( + )
      | Dec_concat ->
          fun l r -> Int.of_string_exn (Int.to_string l ^ Int.to_string r)
  end

  module Test = Test_f (Operation)

  let run (input : string) : (string, string) result =
    Result.guard_str_trace @@ fun () ->
    Test.parse input
    |> List.fold_left ~init:0 ~f:(fun sum ((total, _) as t) ->
           if Test.is_valid t then sum + total else sum)
    |> Int.to_string
end
