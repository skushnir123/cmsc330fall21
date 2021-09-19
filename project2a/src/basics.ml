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

let volume x y = match x,y,z with
| (x1, y1,z1), (x2, y2,z2) -> (x2-x1) * (y2-y1) * (z2-z1)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 

let rec pow x y = failwith "unimplemented"

let rec log x y = failwith "unimplemented"

let rec gcf x y = failwith "unimplemented"

let rec is_prime x = failwith "unimplemented"

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match lst with
  | [] -> failwith "Out of bounds"
  | h::t ->
      if idx == 0 then h
      else get (idx-1) t


let rec get2 idx lst = match lst with
  | [] -> -1
  | h::t ->
      if idx == 0 then 1
      else get2 (idx-1) t

let rec larger_aux lst1 lst2 idx = match get2 idx lst1, get2 idx lst2 with
  | -1, -1 -> []
  | -1, 1 -> lst2
  | 1, -1 -> lst1
  | 1, 1 -> larger_aux lst1 lst2 (idx+1) 
              
              
let rec larger lst1 lst2 = larger_aux lst1 lst2 0

let reverse lst = failwith "unimplemented"

let rec combine lst1 lst2 = failwith "unimplemented"

let rec merge lst1 lst2 = failwith "unimplemented"

let rec rotate shift lst = failwith "unimplemented"

let rec is_palindrome lst = failwith "unimplemented"