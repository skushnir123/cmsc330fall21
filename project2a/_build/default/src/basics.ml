(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with
| (a,b,c) -> (c,b,a)

let is_odd x = match x mod 2 with
  | 0 -> false
  | _ -> true

let area x y = match x,y with
| (x1, y1), (x2, y2) -> (x2-x1) * (y2-y1)

let volume x y = match x,y with
| (x1, y1,z1), (x2, y2,z2) -> (x2-x1) * (y2-y1) * (z2-z1)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = match n with
  | 0 -> 0
  | 1 -> 1
  | x -> fibonacci (x-1) + fibonacci (x-2)

let rec pow x y = match y with
| 0 -> 1
| _ -> x * pow x (y-1)

let rec log x y = match x,y with
| x,y ->
  if x > y then 0
  else 1 + log x (y/x)

let rec gcf x y = match y with
| y ->
  if y = 0 then x
  else gcf y (x mod y)

let rec is_prime_aux x d =
  if d = x then true
  else if (x mod d) = 0 then false
  else is_prime_aux x (d+1)

let rec is_prime x = 
  if x < 3 then false
  else is_prime_aux x 2

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match lst with
  | [] -> failwith "Out of bounds"
  | h::t ->
      if idx = 0 then h
      else get (idx-1) t


let rec get2 idx lst = match lst with
  | [] -> -1
  | h::t ->
      if idx = 0 then 1
      else get2 (idx-1) t

let rec larger_aux lst1 lst2 idx = match get2 idx lst1, get2 idx lst2 with
  | -1, -1 -> []
  | -1, 1 -> lst2
  | 1, -1 -> lst1
  | 1, 1 -> larger_aux lst1 lst2 (idx+1) 
              
              
let rec larger lst1 lst2 = larger_aux lst1 lst2 0

let rec combine_for_reverse lst1 lst2 = match lst1 with
  | [] -> lst2
  | h::t -> h::combine_for_reverse t lst2

let rec reverse_helper lst = match lst with
| [] -> []
| h::t -> combine_for_reverse (reverse_helper t) [h]

let reverse lst = reverse_helper lst

let rec combine lst1 lst2 = match lst1 with
  | [] -> lst2
  | h::t -> h::combine t lst2

let rec merge lst1 lst2 = match lst1, lst2 with
  | [],[] -> []
  | h::t, [] -> lst1
  | [], h::t -> lst2
  | h1::t1, h2::t2 -> 
    if h1 < h2 then h1 :: merge t1 lst2
    else h2 :: merge lst1 t2

let rec rotate shift lst = match lst with
  | h::t -> 
      if shift = 0 then lst
      else rotate (shift-1) (combine t [h])

let rec check_string_equality lst1 lst2 = match lst1, lst2 with
  | [], [] -> true
  | [], h2::t2 -> false
  | h1::t1, [] -> false
  | h1::t1, h2::t2 -> 
    if h1 = h2 then check_string_equality t1 t2
    else false

let rec is_palindrome lst = check_string_equality lst (reverse lst) 
