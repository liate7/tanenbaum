open! Common

let year = 2024
let day = 03

let example =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let number = Re.(repn digit 1 @@ Some 3)

let mul =
  Re.(
    seq
      [
        group @@ str "mul";
        str "(";
        group number;
        str ",";
        group number;
        str ")";
      ])

let mul' = Re.compile mul

module Part_1 = struct
  let get_muls str = Re.all mul' str |> List.map ~f:Re.Group.all

  let run input =
    Result.guard_str @@ fun () ->
    get_muls input
    |> List.map ~f:(function
         | [| _; "mul"; l; r |] -> Int.of_string_exn l * Int.of_string_exn r
         | _ -> assert false)
    |> List.monoid_reduce ~m:Monoid.add
    |> Int.to_string
end

let example_2 =
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let do' = Re.str "do()"
let don't = Re.str "don't()"
let with_conditionals = Re.alt [ mul; do'; don't ]
let with_conditionals' = Re.compile with_conditionals

module Part_2 = struct
  type state = { acc : int; is_enabled : bool }

  let init = { acc = 0; is_enabled = true }
  let get_stmts str = Re.all with_conditionals' str |> List.map ~f:Re.Group.all

  let run input =
    Result.guard_str @@ fun () ->
    get_stmts input
    |> List.fold_left ~init ~f:(fun state -> function
         | [| _; "mul"; l; r |] when state.is_enabled ->
             {
               state with
               acc = state.acc + (Int.of_string_exn l * Int.of_string_exn r);
             }
         | [| _; "mul"; _; _ |] -> state
         | [| "do()"; ""; ""; "" |] -> { state with is_enabled = true }
         | [| "don't()"; ""; ""; "" |] -> { state with is_enabled = false }
         | arr ->
             failwith
             @@ Printf.sprintf {|Unknown stmt: "%a"|}
                  (fun () -> Array.to_string Fun.id)
                  arr)
    |> fun { acc; _ } -> Int.to_string acc
end
