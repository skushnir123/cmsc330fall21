open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a h -> if h = e then true else a) false lst

let is_present lst x = failwith "unimplemented"

let count_occ lst target = failwith "unimplemented"

let uniq lst = failwith "unimplemented"

let assoc_list lst = failwith "unimplemented"

let ap fns args = fold (@) [] (map (fun fn -> map fn args) fns)
