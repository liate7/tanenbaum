open! Common

let year = 2025
let day = 10

let example =
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
   [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
   [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}\n"

(* The manual describes one machine per line. Each line contains a single indicator light
   diagram in [square brackets], one or more button wiring schematics in (parentheses), and
   joltage requirements in {curly braces}.

   To start a machine, its indicator lights must match those shown in the diagram, where
   . means off and # means on. The machine has the number of indicator lights shown, but its
   indicator lights are all initially off.

   You can toggle the state of indicator lights by pushing any of the listed buttons. Each
   button lists which indicator lights it toggles, where 0 means the first light, 1 means the
   second light, and so on. When you push a button, each listed indicator light either turns on
   (if it was off) or turns off (if it was on). You have to push each button an integer number
   of times; there's no such thing as "0.5 presses" (nor can you push a button a negative
   number of times).

   Because none of the machines are running, the joltage requirements are irrelevant and can be
   safely ignored.

   You can push each button as many times as you like. However, to save on time, you will need
   to determine the fewest total presses required to correctly configure all indicator lights
   for all machines in your list.

   What is the fewest button presses required to correctly configure the indicator lights on
   all of the machines? *)

(* There are a few ways to correctly configure the first machine:

   [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}

   - You could press the first three buttons once each, a total of 3 button presses.
   - You could press (1,3) once, (2,3) once, and (0,1) twice, a total of 4 button presses.
   - You could press all of the buttons except (1,3) once each, a total of 5 button presses.

   However, the fewest button presses required is 2. One way to do this is by pressing the last
   two buttons ((0,2) and (0,1)) once each.

   The second machine can be configured with as few as 3 button presses:

   [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}

   One way to achieve this is by pressing the last three buttons ((0,4), (0,1,2), and
   (1,2,3,4)) once each.

   The third machine has a total of six indicator lights that need to be configured correctly:

   [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

   The fewest presses required to correctly configure it is 2; one way to do this is by
   pressing buttons (0,3,4) and (0,1,2,4,5) once each.

   So, the fewest button presses required to correctly configure the indicator lights on all of
   the machines is 2 + 3 + 2 = 7. *)

module Set_of (O : sig
  include Set.OrderedType

  val pp : Format.formatter -> t -> unit
end) : sig
  include Set.S with type t = Set.Make(O).t with type elt = O.t

  val sym_diff : t -> t -> t
  val sym_diff_monoid : t Monoid.t
  val take_drops : int -> t -> (t * t) Iter.t

  val power_set : t -> t Iter.t
  (** Iterates in order of size *)

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end = struct
  include Set.Make (O)

  let sym_diff l r = diff (union l r) (inter l r)

  let rec take_drops n t : (t * t) Iter.t =
    match nat_view n with
    | `Zero -> Iter.singleton (empty, t)
    | `Succ n ->
        to_iter t
        |> Iter.flat_map ~f:(fun x ->
               let t = remove x t in
               take_drops n t
               |> Iter.filter_map ~f:(fun (t, d) ->
                      let min = min_elt_opt t in
                      if
                        Option.map_or ~default:false
                          (fun min -> O.compare min x < 0)
                          min
                      then None
                      else Some (add x t, d)))

  let power_set t : t Iter.t =
    Iter.(0 -- cardinal t)
    |> Iter.flat_map ~f:(fun n -> take_drops n t)
    |> Iter.map ~f:fst

  let sym_diff_monoid = Monoid.{ identity = empty; reduce = sym_diff }
  let pp_hum f t = Format.fprintf f "@[{%a}@]" (List.pp O.pp) (to_list t)
end

module IntSet = Set_of (Int)

module Machine = struct
  type t = { lights_on : IntSet.t; buttons : IntSet.t array; joltage : unit }

  let lights_of_string str =
    str |> String.find_all ~sub:"#" |> Iter.of_gen_once |> IntSet.of_iter

  let button_of_string str =
    String.split_on_char ~by:',' str
    |> List.map ~f:Int.of_string_exn
    |> IntSet.of_list

  let buttons_of_string str =
    String.split_on_char ~by:' ' str
    |> Iter.of_list
    |> Iter.filter ~f:(Fun.negate String.is_empty)
    |> Iter.map ~f:(fun x ->
           x
           |> String.drop_while ~f:Char.(( = ) '(')
           |> String.rdrop_while ~f:Char.(( = ) ')')
           |> button_of_string)
    |> Iter.to_array

  let joltage_of_string _ = ()

  let regex =
    Re.(
      compile
      @@ seq
           [
             seq [ char '['; group ~name:"lights" @@ rep any; char ']' ];
             group ~name:"buttons" @@ rep any;
             seq [ char '{'; group ~name:"joltages" @@ rep any; char '}' ];
           ])

  let lights_idx, buttons_idx, joltage_idx =
    let groups = Re.group_names regex in
    let f name = List.assoc ~eq:String.equal name groups in
    (f "lights", f "buttons", f "joltages")

  let of_string str =
    let res = Re.exec regex str in
    {
      lights_on = Re.Group.get res lights_idx |> lights_of_string;
      buttons = Re.Group.get res buttons_idx |> buttons_of_string;
      joltage = Re.Group.get res joltage_idx |> joltage_of_string;
    }

  let enable_set { lights_on; buttons; joltage = _ } =
    IntSet.of_iter Iter.(0 --^ Array.length buttons)
    |> IntSet.power_set
    |> Iter.filter ~f:(fun idxes ->
           let res =
             IntSet.to_iter idxes
             |> Iter.map ~f:(Array.get buttons)
             |> Iter.reduce ~m:IntSet.sym_diff_monoid
           in
           IntSet.equal res lights_on)
end

module Part_1 = struct
  let parse str = String.lines_iter str |> Iter.map ~f:Machine.of_string

  let unparse presses =
    presses
    |> Iter.map ~f:IntSet.cardinal
    |> Iter.reduce ~m:Monoid.add |> Int.to_string

  let go it = it |> Iter.map ~f:Machine.enable_set |> Iter.map ~f:Iter.head_exn

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
