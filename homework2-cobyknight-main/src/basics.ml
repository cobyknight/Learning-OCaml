let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = 
  match tup with
  | (x,y) -> (y,x)
;;

let rev_triple (tup : 'a * 'b * 'c) = 
  match tup with
  | (x,y,z) -> (z,y,x)
;;

let is_odd x = x mod 2 <> 0;;

let is_older (date1: int * int * int) (date2: int * int * int) = 
  match date1, date2 with
  | (y1, m1, d1), (y2, m2,d2) ->
    if y1 < y2 then true
    else if y1 > y2 then false
    else if m1 < m2 then true
    else if m1 > m2 then false
    else d1 < d2
;;


let to_us_format (date1: int * int * int) =
  match date1 with
  | (y, m, d) -> (m, d, y)
;;
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = 
  if p = 0 then 1
  else if p = 1 then x
  else x * pow x (p - 1)
;;

let rec fac n = 
  if n = 0 then 1
  else n * fac (n - 1)
;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = 
  match (idx, lst) with
  | (0, x::_) -> x
  | (_, []) -> List.hd lst
  | (n, _::rest) -> get_nth (n-1, rest)
;;

let larger lst1 lst2 = 
  if List.length lst1 > List.length lst2 then
    lst1
  else if List.length lst1 < List.length lst2 then
    lst2
  else
    []
;;

let sum lst1 lst2 =
  let sum_list lst = List.fold_left (+) 0 lst in
  sum_list lst1 + sum_list lst2
;;