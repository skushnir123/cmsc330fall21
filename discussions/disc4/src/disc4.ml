(*Provided for your benefit*)
let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)

let rec foldl f a xs = match xs with
| [] -> a
| x :: xt -> foldl f (f a x) xt

let rec foldr f xs a = match xs with
| [] -> a
| x :: xt -> f x (foldr f xt a) 

(********************)
(* Currying Functions and Partial Application *)
(********************)

let mul_n n lst = map (fun x -> x*n) lst

(* Joins together the strings in xs by separator sep
   e.g. join ["cat"; "dog"; "fish"] "," = "cat,dog,fish". *)

let join_aux sep = fun a x -> match a with
| "" -> x
| _ -> a ^ sep ^ x

let join xs sep = foldl (join_aux sep) "" xs

(********************)
(* Option Functions *)
(********************)

(* Converts an option to a list. Some is a singleton containing
   the value, while None is an empty list. *)
let list_of_option (o : 'a option) : 'a list = match o with
  | None -> []
  | Some x -> [x]

(* If the pair's key matches k returns the value as an option. Otherwise
   return None. *)
let match_key (k : 'k) (p : ('k * 'v)) : 'v option = match p with
  | (a,b) ->
      if k = a then Some b
      else None

(******************)
(*LENGTHLIST FUNCTIONS*)
(******************)

(*This list encodes the idea of a list having multiple elements in a row*)
(*Ex: [1;2;3;4] = Cons(1, 1, Cons(2, 1, Cons(3, 1, Cons(4, 1, Empty))))*)
(*Ex: [1; 1; 1; 1; 2; 2; 2; 3; 3; 4] = Cons(1, 4, Cons(2, 3, Cons(3, 2, Cons(4, 1, Empty))))*)

type 'a lengthlist = 
    Cons of ('a * int * 'a lengthlist)
    | Empty
;;

let rec len_help ele num = 
   if num = 0 then [] else ele::(len_help ele (num-1));;

let rec list_of_lengthlist llst = 
   match llst with
   | Empty -> []
   | Cons(a', num, tail) -> (len_help a' num) @ list_of_lengthlist tail;;

let rec map_lengthlist fn llst = match llst with
  | Empty -> Empty
  | Cons(a, num, tail) -> Cons(fn a, num, (map_lengthlist fn tail))

let rec decrement_count llst = 
   match llst with
   | Empty -> Empty
   | Cons(a', num, tail) -> if num = 1 then decrement_count tail else Cons(a', (num-1), (decrement_count tail));;