open Funs

let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt

let rec contains_elem lst e = match lst with
  | [] -> false
  | h::t -> match h with
      (a,b) ->
        if a = e then true
        else contains_elem t e


let rec return_elem lst e = match lst with
  | [] -> 613
  | h::t -> match h with
      (a,b) ->
        if a = e then b
        else return_elem t e

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = match t with
  | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode(x1, None, l, m, r) ->
      if x1 < x then IntNode(x1, Some x, l, m, r)
      else if x1 > x then IntNode(x, Some x1, l, m, r)
      else t
  | IntNode(x1, Some x2, l,m,r) ->
      if x < x1 then IntNode(x1, Some x2, int_insert x l, m, r )
      else if x > x1 && x < x2 then IntNode(x1, Some x2, l, int_insert x m, r )
      else if x > x2 then IntNode(x1, Some x2, l, m, int_insert x r)
      else t


let rec int_mem x t = match t with
  | IntLeaf -> false
  | IntNode(x1, Some x2, l,m,r) ->
      if x = x1 || x = x2 then true
      else if x < x1 then int_mem x l
      else if x > x1 && x < x2 then int_mem x m
      else int_mem x r
  | IntNode(x1, None, l, m, r) ->
    if x = x1 then true else false
  

let rec int_size t = match t with
| IntLeaf -> 0
| IntNode(x1, None, l, m, r) -> 1
| IntNode(x1, Some x2, l,m,r) ->
  2 + (int_size l) + (int_size m) + (int_size r)

  let rec int_max t = match t with
  | IntLeaf -> failwith "sdfs"
  | IntNode(x1, None, l, m, r) -> x1
  | IntNode(x1, Some x2, l,m,r) -> 
      if r = empty_int_tree then x2
      else int_max r

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with
  | MapLeaf -> MapNode((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode((k1,v1), None, l, m, r) ->
      if k < k1 then MapNode((k,v), Some (k1,v1), l, m, r)
      else if k > k1 then MapNode((k1,v1), Some (k,v), l, m, r)
      else invalid_arg "map_put"
  | MapNode((k1,v1), Some (k2, v2), l, m, r) ->
      if k < k1 then MapNode((k1,v1), Some (k2, v2), map_put k v l, m, r)
      else if k > k1 && k < k2 then MapNode((k1,v1), Some (k2, v2), l, map_put k v m, r)
      else if k > k2 then MapNode((k1,v1), Some (k2, v2), l, m, map_put k v r)
      else invalid_arg "map_put"

let rec map_contains k t = match t with
| MapLeaf -> false
| MapNode((k1,v1), None, l, m, r) ->
    if k1 = k then true
    else false
| MapNode((k1,v1), Some (k2, v2), l, m, r) ->
  if k = k1 || k = k2 then true
  else if k < k1 then map_contains k l
  else if k > k1 && k < k2 then map_contains k m
  else map_contains k r

let rec map_get k t = match t with
| MapLeaf -> invalid_arg "map_get"
| MapNode((k1,v1), None, l, m, r) ->
    if k1 = k then v1
    else invalid_arg "map_get"
| MapNode((k1,v1), Some (k2, v2), l, m, r) ->
  if k = k1 then v1
  else if k = k2 then v2
  else if k < k1 then map_get k l
  else if k > k1 && k < k2 then map_get k m
  else map_get k r
  

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
| EmptyTable
| Scope of (((string) * (int)) list * lookup_table)

let empty_table : lookup_table = EmptyTable

          
let push_scope (table : lookup_table) : lookup_table = 
  match table with
    EmptyTable -> Scope([], EmptyTable)
  | Scope(arr, parent_scope) ->
      Scope([], table)

let pop_scope (table : lookup_table) : lookup_table =
  match table with
  EmptyTable -> failwith "No scopes remain!"
  | Scope(arr, parent_scope) -> parent_scope

let add_var name value (table : lookup_table) : lookup_table =
  match table with
  | EmptyTable -> failwith "There are no scopes to add a variable to!"
  | Scope(arr, parent_scope) -> 
      if (contains_elem arr name) = true then failwith "Duplicate variable binding in scope!"
      else 
        let new_arr = arr @ [(name, value)] in
        Scope(new_arr, parent_scope)


let rec lookup name (table : lookup_table) = match table with
| EmptyTable -> failwith "Variable not found!"
| Scope(arr, parent_scope) ->
  if (return_elem arr name) = 613 then lookup name parent_scope
  else return_elem arr name
  
