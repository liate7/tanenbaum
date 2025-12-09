open! Common

let year = 2025
let day = 2

let example =
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

(* The ranges are separated by commas (,); each range gives its first ID and last ID separated
   by a dash (-).

   Since the young Elf was just doing silly patterns, you can find the invalid IDs by looking
   for any ID which is made only of some sequence of digits repeated twice. So, 55 (5 twice),
   6464 (64 twice), and 123123 (123 twice) would all be invalid IDs.

   None of the numbers have leading zeroes; 0101 isn't an ID at all. (101 is a valid ID that
   you would ignore.)

   Your job is to find all of the invalid IDs that appear in the given ranges.

   What do you get if you add up all of the invalid IDs?  *)

module Prefix = struct
  let of_str num =
    if String.length num mod 2 = 0 then
      Int.of_string (String.take (String.length num / 2) num)
    else None

  let upper_bound lower =
    let expt = Int.to_float lower |> log10 |> ceil in
    (10. ** expt |> Int.of_float) - 1

  let lower_bound upper =
    let expt = Int.to_float upper |> log10 |> floor in
    10. ** expt |> Int.of_float

  let to_int int = Int.of_string_exn @@ Printf.sprintf "%d%d" int int
end

module Range = struct
  type 'a t = { lwb : 'a; upb : 'a }

  let of_string inner str =
    let[@warning "-8"] [ lwb; upb ] = String.split_on_char ~by:'-' str in
    { lwb = inner lwb; upb = inner upb }

  let contains n { lwb; upb } = Int.(lwb <= n && n <= upb)

  let prefixes { lwb; upb } : int Seq.t =
    match (Prefix.of_str lwb, Prefix.of_str upb) with
    | None, None -> Seq.empty
    | Some lower, None -> Seq.(lower -- Prefix.upper_bound lower)
    | None, Some upper -> Seq.(Prefix.lower_bound upper -- upper)
    | Some lower, Some upper -> Seq.(lower -- upper)
end

let parse str =
  String.trim str |> String.split ~by:","
  |> List.map ~f:(fun r ->
         (Range.of_string Int.of_string_exn r, Range.of_string Fun.id r))

(* In the above example:

    11-22 has two invalid IDs, 11 and 22.
    95-115 has one invalid ID, 99.
    998-1012 has one invalid ID, 1010.
    1188511880-1188511890 has one invalid ID, 1188511885.
    222220-222224 has one invalid ID, 222222.
    1698522-1698528 contains no invalid IDs.
    446443-446449 has one invalid ID, 446446.
    38593856-38593862 has one invalid ID, 38593859.
    The rest of the ranges contain no invalid IDs.

  Adding up all the invalid IDs in this example produces 1227775554. *)
module Part_1 = struct
  (* Brainstorming:
     - probably want a generator of number /strings/ then convert for addition
     - there's only one possible invalid ID per prefix, so can just calculate prefixen
       then check for existence
     - also only matter when ⌊log_10 n⌋ is even *)

  let go vals =
    let f (range, strs) =
      Range.prefixes strs
      |> Seq.filter_map (fun prefix ->
             Prefix.to_int prefix
             |> Option.if_ (fun n -> Range.contains n range))
      |> Seq.fold_left ( + ) 0
    in
    vals |> List.monoid_map_reduce ~f ~m:Monoid.add

  let run (input : string) : (string, string) result =
    Ok (parse input |> go |> Int.to_string)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
