open! Common

let year = 2025
let day = 5
let example = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32\n"

(* The database operates on ingredient IDs. It consists of a list of fresh ingredient ID
   ranges, a blank line, and a list of available ingredient IDs.

   The fresh ID ranges are inclusive: the range 3-5 means that ingredient IDs 3, 4, and 5 are
   all fresh. The ranges can also overlap; an ingredient ID is fresh if it is in any range.

   The Elves are trying to determine which of the available ingredient IDs are fresh.

   How many of the available ingredient IDs are fresh? *)

(* In this example, this is done as follows:

    Ingredient ID 1 is spoiled because it does not fall into any range.
    Ingredient ID 5 is fresh because it falls into range 3-5.
    Ingredient ID 8 is spoiled.
    Ingredient ID 11 is fresh because it falls into range 10-14.
    Ingredient ID 17 is fresh because it falls into range 16-20 as well as range 12-18.
    Ingredient ID 32 is spoiled.

   So, in this example, 3 of the available ingredient IDs are fresh. *)

module Range : sig
  type t

  val contains : t -> int -> bool

  val ( -- ) : int -> int -> t
  (** Inclusive range constructor *)

  val ( --^ ) : int -> int -> t
  (** Half-open range constructor. [l --^ r] is like \[l, r\) *)

  val intersect : t -> t -> bool

  val ( <> ) : t -> t -> [ `Merged of t | `LR of t * t | `RL of t * t ]
  (** [l <> r] either creates a new range that exactly covers [l] and [r], or
      returns them sorted *)
end = struct
  type t = { lo : int; hi : int }
  (** Internally, is an /inclusive/ range. Invariant: [lo <= hi] *)

  let contains { lo; hi } x = lo <= x && x <= hi

  let ( -- ) lo hi =
    assert (lo <= hi);
    { lo; hi }

  let ( --^ ) lo hi =
    assert (lo < hi);
    { lo; hi = hi - 1 }

  let intersect l r =
    contains r l.lo || contains r l.hi || contains l r.lo || contains l r.hi

  let ( <> ) l r =
    if intersect l r then
      let lo = min l.lo r.lo and hi = max l.hi r.hi in
      `Merged { lo; hi }
    else if l.lo <= r.lo then `LR (l, r)
    else `RL (r, l)
end

module DB = struct
  type t = Range.t list
  (** Invariant: is sorted *)

  let rec merge l r =
    match (l, r) with
    | [], r | r, [] -> r
    | l :: ls, r :: rs -> (
        match Range.(l <> r) with
        | `Merged x -> x :: merge ls rs
        | `LR (l, r) -> l :: merge ls (r :: rs)
        | `RL (r, l) -> r :: merge (l :: ls) rs)

  let monoid = Monoid.{ identity = []; reduce = merge }
  let of_iter it = Iter.(it |> map ~f:(fun x -> [ x ]) |> reduce ~m:monoid)
  let of_list ls = List.to_iter ls |> of_iter
  let contains t x = List.exists ~f:(Fun.flip Range.contains x) t
end

let range_of_string str =
  let[@warning "-8"] [ l; r ] = String.split_on_char ~by:'-' str in
  Range.(Int.of_string_exn l -- Int.of_string_exn r)

let parse str =
  let[@warning "-8"] ranges, _empty :: ids =
    String.lines str |> List.take_drop_while ~f:(Fun.negate String.is_empty)
  in
  let db = Iter.(of_list ranges |> map ~f:range_of_string |> DB.of_iter) in
  (db, Iter.(of_list ids |> map ~f:Int.of_string_exn))

let unparse = Int.to_string

module Part_1 = struct
  let go (db, ids) = Iter.(ids |> filter_count ~f:(DB.contains db))

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
