include ContainersLabels

let ( <| ) = ( @@ )
let ( %> ) = Fun.( %> )

module IntMap = Map.Make (Int)

module Monoid = struct
  type 'a t = { identity : 'a; reduce : 'a -> 'a -> 'a }

  let mul = { identity = 1; reduce = ( * ) }
  let add = { identity = 0; reduce = ( + ) }
end

module List = struct
  include CCListLabels

  let pairs t =
    let rec go = function
      | ([] | [ _ ]) as ret -> Error ret
      | x :: y :: rest ->
          Ok ((x, y) :: Result.get_or ~default:[] (go (y :: rest)))
    in
    go t

  let monoid_reduce ~(m : 'a Monoid.t) ls =
    List.fold_left ~init:m.identity ~f:m.reduce ls

  let monoid_map_reduce ~(m : 'a Monoid.t) ~f ls =
    List.fold_on_map ~reduce:m.reduce ~f ~init:m.identity ls
end

module String = struct
  include Stringext
  include CCStringLabels
end

module IntIntOrder = struct
  type t = int * int

  let compare =
    Fun.lexicographic
      (fun (x, _) (x', _) -> Int.compare x x')
      (fun (_, y) (_, y') -> Int.compare y y')
end

let nat_view = function
  | 0 -> `Zero
  | n when n > 0 -> `Succ (n - 1)
  | n -> failwith @@ Printf.sprintf "not a nat: %d" n

module Iter = struct
  include IterLabels

  let reduce ~(m : 'a Monoid.t) ls =
    IterLabels.fold ~init:m.identity ~f:m.reduce ls

  let ( --^ ) l r = l -- (r - 1)
end

module Point2d = struct
  type t = { x : int; y : int }

  let of_string str =
    let open Option.Infix in
    match String.split_on_char ~by:',' str with
    | [ x; y ] ->
        let+ x = Int.of_string x and+ y = Int.of_string y in
        { x; y }
    | _ -> None

  let of_pair (x, y) = { x; y }
  let to_pair { x; y } = (x, y)
  let origin = { x = 0; y = 0 }

  let magn { x; y } =
    let sqr x = Float.of_int x ** 2. in
    sqrt (sqr x +. sqr y)

  let map f { x; y } = { x = f x; y = f y }
  let map2 f l r = { x = f l.x r.x; y = f l.y r.y }
  let dist l r = magn @@ map2 ( - ) l r
  let map_to_pair f { x; y } = (f x, f y)
  let map_x f { x; y } = { x = f x; y }
  let map_y f { x; y } = { x; y = f y }

  let compare =
    Fun.lexicographic
      (fun { x = l; _ } { x = r; _ } -> Int.compare l r)
      (fun { y = l; _ } { y = r; _ } -> Int.compare l r)
end

module Point3d = struct
  type t = { x : int; y : int; z : int }

  let of_string str =
    let open Option.Infix in
    match String.split_on_char ~by:',' str with
    | [ x; y; z ] ->
        let+ x = Int.of_string x
        and+ y = Int.of_string y
        and+ z = Int.of_string z in
        { x; y; z }
    | _ -> None

  let origin = { x = 0; y = 0; z = 0 }

  let magn { x; y; z } =
    let sqr x = Float.of_int x ** 2. in
    sqrt (sqr x +. sqr y +. sqr z)

  let map f { x; y; z } = { x = f x; y = f y; z = f z }
  let map2 f l r = { x = f l.x r.x; y = f l.y r.y; z = f l.z r.z }
  let dist l r = magn @@ map2 ( - ) l r
end

module Range : sig
  type t = private { lo : int; hi : int }
  (** This is an /inclusive/ range. Invariant: [lo <= hi] *)

  val contains : t -> int -> bool

  val ( -- ) : int -> int -> t
  (** Inclusive range constructor *)

  val ( --^ ) : int -> int -> t
  (** Half-open range constructor. [l --^ r] is like \[l, r\) *)

  val intersect : t -> t -> bool

  val ( <> ) : t -> t -> [ `Merged of t | `LR of t * t | `RL of t * t ]
  (** [l <> r] either creates a new range that exactly covers [l] and [r], or
      returns them sorted *)

  val to_iter : t -> int Iter.t
  val length : t -> int
end = struct
  type t = { lo : int; hi : int }

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

  let to_iter { lo; hi } = Iter.(lo -- hi)
  let length { lo; hi } = hi - lo + 1
end

let sub x = x - 1
let inc x = x + 1
