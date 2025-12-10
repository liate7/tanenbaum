open! Common

let year = 2025
let day = 3

let example =
  "987654321111111\n811111111111119\n234234234234278\n818181911112111\n"

(* The batteries are arranged into banks; each line of digits in your input corresponds to a
   single bank of batteries. Within each bank, you need to turn on exactly two batteries; the
   joltage that the bank produces is equal to the number formed by the digits on the batteries
   you've turned on. For example, if you have a bank like 12345 and you turn on batteries 2 and
   4, the bank would produce 24 jolts. (You cannot rearrange batteries.)

   You'll need to find the largest possible joltage each bank can produce.

   The total output joltage is the sum of the maximum joltage from each bank[.] *)

(* In the above example:

    In 987654321111111, you can make the largest joltage possible, 98, by turning on the first
      two batteries.
    In 811111111111119, you can make the largest joltage possible by turning on the batteries
      labeled 8 and 9, producing 89 jolts.
    In 234234234234278, you can make 78 by turning on the last two batteries (marked 7 and 8).
    In 818181911112111, the largest joltage you can produce is 92.

   [I]n this example, the total output joltage is 98 + 89 + 78 + 92 = 357. *)

module Dequeue = CCFQueue

module Bank = struct
  type t = int Dequeue.t

  let of_string str : t =
    String.to_iter str
    |> Iter.map ~f:Fun.(Char.to_string %> Int.of_string_exn)
    |> Dequeue.of_iter

  let max_joltage ?(banks = 2) (t : t) : int =
    let rec go : t -> int -> int -> int =
     fun t acc banks ->
      match nat_view banks with
      | `Succ banks' ->
          let next =
            Dequeue.take_back_l banks' t
            |> fst |> Dequeue.to_iter |> Iter.max_exn ~lt:( < )
          in
          let rest =
            Dequeue.take_front_while (( <> ) next) t
            |> snd |> Dequeue.take_front_exn |> snd
          in
          go rest (next + (10 * acc)) banks'
      | `Zero -> acc
    in
    go t 0 banks
end

let parse str =
  str |> String.trim |> String.lines_iter |> Iter.map ~f:Bank.of_string

module Part_1 = struct
  let go banks =
    banks |> Iter.map ~f:Bank.max_joltage |> Iter.reduce ~m:Monoid.add

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> Int.to_string
end

(* Now, you need to make the largest joltage by turning on exactly twelve batteries within each
   bank. *)

module Part_2 = struct
  let go banks =
    banks
    |> Iter.map ~f:(Bank.max_joltage ~banks:12)
    |> Iter.reduce ~m:Monoid.add

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> Int.to_string
end
