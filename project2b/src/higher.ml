open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a x -> if x = e then true else a) false lst

let is_present lst x = map (fun h -> if h = x then 1 else 0) lst

let count_occ lst target = fold (fun a h -> if h = target then (a+1) else a) 0 lst

let uniq lst = fold_right (fun h a -> if (contains_elem a h) then a else h::a) lst []

let assoc_list lst = map (fun h -> (h, count_occ lst h)) (uniq lst)

let ap fns args = fold (@) [] (map (fun fn -> map fn args) fns)
