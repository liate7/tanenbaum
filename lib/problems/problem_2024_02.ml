open! Common

let year = 2024
let day = 2

let example =
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9\n"

module Report = struct
  type t = int list

  let check dir l r =
    let dist = abs (l - r) in
    dir l r && dist <= 3 && dist > 0

  let is_safe t =
    match List.pairs t with
    | Error _ -> true
    | Ok ((l, r) :: _ as pairs) when l < r ->
        List.for_all ~f:(Fun.uncurry @@ check ( < )) pairs
    | Ok pairs -> List.for_all ~f:(Fun.uncurry @@ check ( > )) pairs

  let dampen t =
    let rec go = function
      | _, (([] | [ _ ]) as ret) -> [ ret ]
      | Some dir, l :: r :: rest when check dir l r ->
          List.map ~f:(List.cons l) @@ go (Some dir, r :: rest)
      | None, l :: r :: rest when check ( < ) l r || check ( > ) l r ->
          (r :: rest)
          :: (List.map ~f:(List.cons l)
             @@ go
                  ( (if check ( < ) l r then Some ( < ) else Some ( > )),
                    r :: rest ))
      | _, l :: r :: rest -> [ l :: rest; r :: rest ]
    in
    go (None, t)

  let is_safe_with_dampener t = dampen t |> List.exists ~f:is_safe

  let parse input =
    String.trim input |> String.lines
    |> List.map ~f:(fun s ->
           String.trim s |> String.split ~by:" "
           |> List.map ~f:Int.of_string_exn)
end

module Part_1 = struct
  let run input : (string, string) result =
    Result.guard_str @@ fun () ->
    Report.parse input |> List.count ~f:Report.is_safe |> Int.to_string
end

module Part_2 = struct
  let run input =
    Result.guard_str @@ fun () ->
    Report.parse input
    |> List.count ~f:Report.is_safe_with_dampener
    |> Int.to_string
end
