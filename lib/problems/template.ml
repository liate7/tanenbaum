open! Common

(* let year = _ *)
(* let day = _ *)
let example = ""

(* Problem description *)

(* Example run-through *)

module Part_1 = struct
  let parse = Fun.id
  let unparse = Fun.id
  let go = Fun.id

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
