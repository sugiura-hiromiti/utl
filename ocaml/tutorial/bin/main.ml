assert (string_of_int (10 : int) = "10");;

let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1)

let inc = fun x -> x + 1;;

assert (3 |> inc |> even);;

let named_arg ~name1 ~name2 = name1 + name2;;

assert (named_arg ~name1:3 ~name2:4 = 7);;

let named_arg ?name:(arg1 = 8) arg2 = arg1 + arg2;;

assert (named_arg ~name:4 9 = 13);
assert (named_arg 9 = 17)
;;

let inc = ( + ) 1;;

assert (inc 9 = 10);;

let rec count_aux n acc = if n = 0 then acc else count_aux (n - 1) (acc + 1)

(** doc comment *)
let count n = count_aux n 0;;

assert (count 7 = 7);;
Printf.printf "%s: %F\n%!" "abc" 5.;;

let rec sum lst = match lst with [] -> 0 | h :: t -> h + sum t;;

assert ([ 3; 2; 99 ] |> sum = 104);;

let inc_first lst = match lst with [] -> [] | h :: t -> (h + 1) :: t;;

assert (inc_first [ 0; 2; 3; 4 ] = [ 1; 2; 3; 4 ]);;

let ( -- ) i j =
  let rec range i j acc = if i > j then acc else range i (j - 1) (j :: acc) in
  range i j []
;;

assert (2 -- 9 = [ 2; 3; 4; 5; 6; 7; 8; 9 ]);;

type lang = Rust | Scheme

let _ = Rust
let lang_of_int l = match l with Rust -> 0 | Scheme -> 1;;

assert (Scheme |> lang_of_int = 1)
