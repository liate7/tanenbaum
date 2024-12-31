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

module Test = struct
  type t = int * int list
  type operation = Times | Plus

  let operation_func = function Times -> ( * ) | Plus -> ( + )

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

  (* let operations = [ ( * ); ( + )] *)

  let operations (total, to_combine) =
    let open Option.Infix in
    let rec go acc = function
      | [] when acc = total -> Some []
      | [] -> None
      | i :: is when i * acc <= total ->
          go (i * acc) is
          >|= List.cons Times
          |> Option.or_lazy ~else_:(fun () ->
                 go (i + acc) is >|= List.cons Plus)
      | i :: is when i + acc <= total -> go (i + acc) is >|= List.cons Plus
      | _ -> None
    in
    match to_combine with i :: is -> go i is | [] -> go 0 []

  let is_valid t = Option.is_some @@ operations t
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    Result.guard_str_trace @@ fun () ->
    Test.parse input
    |> List.fold_left ~init:0 ~f:(fun sum ((total, _) as t) ->
           if Test.is_valid t then sum + total else sum)
    |> Int.to_string
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
