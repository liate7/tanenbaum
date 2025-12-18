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

module type Arg = sig
  include Set.OrderedType

  val pp : Format.formatter -> t -> unit
end

module Set_of (O : Arg) : sig
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

module Bag_of (O : Arg) : sig
  type t
  type elem = O.t

  val empty : t
  val of_counts_iter : (elem * int) Iter.t -> t
  val of_set : Set_of(O).t -> t
  val of_list : elem list -> t
  val cardinal : t -> int
  val equal : t -> t -> bool
  val sum : t -> t -> t
  val mul : t -> int -> t
  val incr : elem -> t -> t
  val decr : elem -> t -> t

  val includes : t -> t -> bool
  (** [includes l r] means [l] ⊆ [r] *)

  val compare : t -> t -> int
  val sum_monoid : t Monoid.t
  val to_iter : t -> (elem * int) Iter.t
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end = struct
  module Map = Map.Make (O)
  module Set = Set_of (O)

  type elem = O.t

  type t = int Map.t
  (** Invariant: ∀ [x : elem], [t : t]. [Map.get x t] > 0 *)

  let empty = Map.empty

  let of_set : Set_of(O).t -> t =
   fun set -> Set.to_iter set |> Iter.map ~f:(fun x -> (x, 1)) |> Map.of_iter

  let of_list = List.map ~f:(fun x -> (x, 1)) %> Map.of_list
  let of_counts_iter = Map.of_iter
  let to_iter = Map.to_iter
  let sum = Map.union (fun _ l r -> Some (l + r))
  let sum_monoid = Monoid.{ identity = empty; reduce = sum }
  let equal = Map.equal Int.equal
  let cardinal t = Map.fold (fun _ l r -> l + r) t 0
  let incr x = Map.update x (function None -> Some 1 | Some n -> Some (inc n))

  let decr x =
    Map.update x
      (Option.flat_map (fun x -> if x > 1 then Some (sub x) else None))

  let mul t x =
    assert (x > 0);
    Map.map (( * ) x) t

  let compare l r = Map.compare Int.compare l r

  let includes smaller bigger =
    Map.to_iter smaller
    |> Iter.for_all ~f:(fun (k, v) -> v <= Map.get_or k bigger ~default:0)

  let print_count f (x, count) =
    assert (count >= 1);
    if count = 1 then Format.fprintf f "%a" O.pp x
    else Format.fprintf f "@[%a:%d@]" O.pp x count

  let pp_hum f t =
    Format.fprintf f "@[{%a}@]" (List.pp print_count) (Map.to_list t)
end

module IntSet = Set_of (Int)
module IntBag = Bag_of (Int)

let bfs : type vertex.
    vertex ->
    (vertex -> vertex list) ->
    (vertex -> vertex -> int) ->
    (vertex -> bool) ->
    vertex list option =
 fun start neighbors compare is_goal ->
  let module Set = Set.Make (struct
    type t = vertex

    let compare = compare
  end) in
  let rec go seen q =
    match CCFQueue.take_front q with
    | None -> None
    | Some ((next, path), _) when is_goal next -> Some path
    | Some ((next, path), q) ->
        let neighbors =
          neighbors next |> List.filter ~f:(fun v -> not @@ Set.mem v seen)
        in
        let seen = Set.add_list seen neighbors in
        let q =
          Iter.of_list neighbors
          |> Iter.map ~f:(fun v -> (v, next :: path))
          |> CCFQueue.add_iter_back q
        in
        go seen q
  in
  go Set.empty @@ CCFQueue.singleton (start, [])

module Machine = struct
  type t = {
    lights_on : IntSet.t;
    buttons : IntSet.t array;
    joltage : IntBag.t;
  }

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

  let joltage_of_string str =
    String.split_on_char ~by:',' str
    |> List.to_iter
    |> Iter.map ~f:Int.of_string_exn
    |> Iter.zip_i |> IntBag.of_counts_iter

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

  let lights_enable_set { lights_on; buttons; joltage = _ } =
    IntSet.of_iter Iter.(0 --^ Array.length buttons)
    |> IntSet.power_set
    |> Iter.filter ~f:(fun idxes ->
           let res =
             IntSet.to_iter idxes
             |> Iter.map ~f:(Array.get buttons)
             |> Iter.reduce ~m:IntSet.sym_diff_monoid
           in
           IntSet.equal res lights_on)

  let joltage_enable_set { lights_on = _; buttons; joltage = target } =
    let vars =
      Array.mapi
        ~f:(fun i b -> (b, Lp.var ~integer:true @@ Format.sprintf "_%d" i))
        buttons
    in
    let eqs =
      IntBag.to_iter target
      |> Iter.map ~f:(fun (sys, count) ->
             Lp.(
               c (Float.of_int count)
               =~ (vars
                  |> Array.filter_map ~f:(fun (b, var) ->
                         if IntSet.mem sys b then Some var else None)
                  |> concat)))
      |> Iter.to_rev_list
    in
    let result = Lp.concat @@ Array.map ~f:snd vars in
    let problem = Lp.make Lp.(minimize result) eqs in
    match Lp_glpk.solve problem with
    | Ok (_, pmap) -> Lp.compute_poly pmap result |> Float.to_int
    | Error msg -> failwith @@ Printf.sprintf "Failed to solve: %s" msg
end

let parse str = String.lines_iter str |> Iter.map ~f:Machine.of_string

module Part_1 = struct
  let unparse presses =
    presses
    |> Iter.map ~f:IntSet.cardinal
    |> Iter.reduce ~m:Monoid.add |> Int.to_string

  let go it =
    it |> Iter.map ~f:Machine.lights_enable_set |> Iter.map ~f:Iter.head_exn

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end

(* All of the machines are starting to come online! Now, it's time to worry about the joltage
   requirements.

   Each machine needs to be configured to exactly the specified joltage levels to function
   properly. Below the buttons on each machine is a big lever that you can use to switch the
   buttons from configuring the indicator lights to increasing the joltage levels. (Ignore the
   indicator light diagrams.)

   The machines each have a set of numeric counters tracking its joltage levels, one counter
   per joltage requirement. The counters are all initially set to zero.

   [I]n this new joltage configuration mode, each button now indicates which counters it
   affects, where 0 means the first counter, 1 means the second counter, and so on. When you
   push a button, each listed counter is increased by 1.

   What is the fewest button presses required to correctly configure the joltage level counters
   on all of the machines? *)

(* Configuring the first machine's counters requires a minimum of 10 button presses. One way to
   do this is by pressing (3) once, (1,3) three times, (2,3) three times, (0,2) once, and (0,1)
   twice.

   Configuring the second machine's counters requires a minimum of 12 button presses. One way
   to do this is by pressing (0,2,3,4) twice, (2,3) five times, and (0,1,2) five times.

   Configuring the third machine's counters requires a minimum of 11 button presses. One way to
   do this is by pressing (0,1,2,3,4) five times, (0,1,2,4,5) five times, and (1,2) once.

   So, the fewest button presses required to correctly configure the joltage level counters on
   all of the machines is 10 + 12 + 11 = 33. *)

module Part_2 = struct
  let unparse presses = presses |> Iter.reduce ~m:Monoid.add |> Int.to_string
  let go it = it |> Iter.map ~f:Machine.joltage_enable_set

  let run (input : string) : (string, string) result =
    Result.guard_str @@ fun () -> parse input |> go |> unparse
end
