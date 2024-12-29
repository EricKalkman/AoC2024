type direction = N | NE | E | SE | S | SW | W | NW [@@deriving ord, show]

let int_of_direction = function
  | N -> 0
  | E -> 1
  | S -> 2
  | W -> 3
  | NE -> 4
  | SE -> 5
  | SW -> 6
  | NW -> 7

let direction_of_int = function
  | 0 -> N
  | 1 -> E
  | 2 -> S
  | 3 -> W
  | 4 -> NE
  | 5 -> SE
  | 6 -> SW
  | 7 -> NW
  | _ -> failwith "bad int"

let dirs4 = [ N; E; S; W ]
let dirs8 = [ N; NE; E; SE; S; SW; W; NW ]

let turn_right = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N
  | NE -> SE
  | SE -> SW
  | SW -> NW
  | NW -> NE

let turn_left = function
  | N -> W
  | W -> S
  | S -> E
  | E -> N
  | NW -> SW
  | SW -> SE
  | SE -> NE
  | NE -> NW

module type Arithmetic = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val one : t
  val minus_one : t
  val zero : t
  val of_int : int -> t
  val compare : t -> t -> int
end

module AInt = struct
  include Int

  let of_int (x : t) = x
end

module type S = sig
  type e
  type t = e * e

  val add : t -> t -> t
  val ( @+ ) : t -> t -> t
  val sub : t -> t -> t
  val ( @- ) : t -> t -> t
  val scale : t -> e -> t
  val ( @* ) : t -> e -> t
  val inv_scale : t -> e -> t
  val ( @/ ) : t -> e -> t
  val step : ?n:e -> direction -> t
  val move : ?n:e -> direction -> t -> t
  val dir_of : t -> direction
  val compare : t -> t -> int
end

module Make2 (A : Arithmetic) : S with type e := A.t = struct
  type e = A.t [@@deriving ord]
  type t = e * e [@@deriving ord]

  let add (x1, y1) (x2, y2) = (A.add x1 x2, A.add y1 y2)
  let ( @+ ) = add
  let sub (x1, y1) (x2, y2) = (A.sub x1 x2, A.sub y1 y2)
  let ( @- ) = sub
  let scale (x, y) k = (A.mul x k, A.mul y k)
  let ( @* ) = scale
  let inv_scale (x, y) k = (A.div x k, A.div y k)
  let ( @/ ) = inv_scale

  let step ?(n = A.one) dir =
    match dir with
    | N -> (A.mul A.minus_one n, A.zero)
    | E -> (A.zero, n)
    | S -> (n, A.zero)
    | W -> (A.zero, A.mul A.minus_one n)
    | NE -> (A.mul A.minus_one n, n)
    | SE -> (n, n)
    | SW -> (n, A.mul A.minus_one n)
    | NW -> (A.mul A.minus_one n, A.mul A.minus_one n)

  let move ?(n = A.one) dir coord =
    let delta = step ~n dir in
    coord @+ delta

  let dir_of (row, col) =
    match (A.compare row A.zero, A.compare col A.zero) with
    | -1, 0 -> N
    | 0, 1 -> W
    | 1, 0 -> S
    | 0, -1 -> E
    | -1, 1 -> NE
    | 1, 1 -> SE
    | 1, -1 -> SW
    | -1, -1 -> NE
    | 0, 0 -> failwith "is zero"
    | _ -> failwith "unreachable"
end

module Ints2 = Make2 (AInt)
module Floats2 = Make2 (Float)
module Ints2Set = Set.Make (Ints2)
module Ints2Map = Map.Make (Ints2)

module Heading = struct
  type t = Ints2.t * direction [@@deriving ord]
end

module HSet = Set.Make (Heading)
