let rec sum = function [] -> 0 | x :: xs -> x + sum xs

type rcrd = { a : string; b : int }

let rcrd a b = { a; b }
let new_rcrd = { a = ""; b = 0 }
let unwrap o = match o with Some i -> i | None -> failwith "option is none"

let rec list_max l =
  match l with
  | [] -> None
  | x :: xs -> (
      match list_max xs with None -> Some x | Some n -> Some (max x n))

type point = float * float
type shape = Point of point | Circle of point * float | Rect of point * point

let area s =
  match s with
  | Point _ -> 0.
  | Circle (_, r) -> r *. r *. Float.pi
  | Rect ((x1, y1), (x2, y2)) -> (x1 -. x2) *. (y1 -. y2) |> abs_float

let center s =
  match s with
  | Point p -> p
  | Circle (p, _) -> p
  | Rect ((x1, y1), (x2, y2)) ->
      let half a b = (a +. b) /. 2. in
      (half x1 x2, half y1 y2)

type 'a mylist = Nil | Cons of 'a * 'a mylist

(* type intlist = Nil | Cons of int * intlist *)
type intlist = int mylist

let rec string_of_intlist l =
  match l with
  | Nil -> "Nil"
  | Cons (x, l') ->
      String.concat ""
        [ "Cons("; string_of_int x; ", "; string_of_intlist l'; ")" ]

let rec intlist_of_list l =
  match l with
  | [] -> Nil
  | x :: xs ->
      (*intlist_of_list_tr xs (Cons (x, acc))*) Cons (x, intlist_of_list xs)

let length il =
  let rec length_tr il acc =
    match il with Nil -> acc | Cons (_, il') -> length_tr il' (acc + 1)
  in
  length_tr il 0

type ('a, 'b) pair = { fst : 'a; snd : 'b }

let f n = match n with 0 -> `Infinite | 1 -> `Finite | n -> `NegFinite (-n)
let f_inv n = match f n with `Infinite -> 0 | `Finite -> 1 | `NegFinite n -> n

type 'a tree = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a tree; right : 'a tree }

let rec mem x l =
  match l with
  | Leaf -> false
  | Node { value; left; right } -> value = x || mem x left || mem x right

let preorder t =
  let rec preorder_tr t acc =
    match t with
    | Leaf -> acc
    | Node { value; left; right } ->
        value :: preorder_tr left (preorder_tr right acc)
  in
  preorder_tr t []

type nat = Zero | Succ of nat

let is_zero n = n = Zero
let pred n = match n with Zero -> None | Succ m -> Some m
let rec add n1 n2 = match n1 with Zero -> n2 | Succ m -> add m (Succ n2)
let rec nat_of_int n = match n with Zero -> 0 | Succ m -> 1 + nat_of_int m

let rec int_of_nat i =
  match i with 0 -> Zero | i' -> Succ (int_of_nat (i' - 1))

module type ModType = sig
  val a : int

  type rgb
end

module Moddd : ModType = struct
  let a = 0

  type rgb = Red | Green | Blue
end
