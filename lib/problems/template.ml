open! Common

let year = failwith "year, eg 2025"
let day = failwith "day /of AoC/"
let example = ""

(* Problem description *)

(* Example run-through *)

let parse = Fun.id
let unparse = Fun.id

module Part_1 = struct
  let go = Fun.id

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* Problem description update *)

(* Example run-through, again *)

module Part_2 = struct
  let go = Fun.id

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
