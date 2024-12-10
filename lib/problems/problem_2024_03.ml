open! Common

let year = 2024
let day = 03

let example =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let number = Re.(repn digit 1 @@ Some 3)

let muls =
  Re.compile
  @@ Re.(seq [ str "mul("; group number; str ","; group number; str ")" ])

module Part_1 = struct
  let get_muls str = Re.all muls str |> List.map ~f:Re.Group.all

  let run input =
    Result.guard_str @@ fun () ->
    get_muls input
    |> List.map ~f:(function
         | [| _; l; r |] -> Int.of_string_exn l * Int.of_string_exn r
         | _ -> assert false)
    |> List.monoid_reduce ~m:Monoid.add
    |> Int.to_string
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
