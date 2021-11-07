open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt

let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)

(* let rec move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = match nfa.delta, qs with
| _, [] -> []
| [], h::t -> move nfa t s
| (start, tran, ending)::t2, h::t ->  
  let newMove = {qs= nfa.qs; sigma= nfa.sigma; delta = t2; q0= nfa.q0; fs= nfa.fs} in
  if h=start & tran = s then ending::move newMove qs s
  else move newMove qs s *)







let x_in_y x y = fold (fun a h -> if h = x then true else a) false y

let add_x_to_y x y = fold (fun a h -> if (x_in_y h a) = true then a else h::a) y x

let rec move_helper nfa h s = 
  fold (fun a (start, tran, ending) -> if tran = s && start = h then ending::a else a) [] nfa.delta

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
fold (fun a h -> union (move_helper nfa h s) a) [] qs

let rec e_closure_aux (nfa: ('q,'s) nfa_t) (qs: 'q) (visited: 'q list) : 'q list = 
  if x_in_y qs visited then []
  else
    let visited = qs::visited in
    let moveNodes = move nfa [qs] None in
    fold (fun a h -> a@(e_closure_aux nfa h visited)) [qs] moveNodes


  

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  fold (fun a h -> let curr_closure = (e_closure_aux nfa h []) in (add_x_to_y curr_closure a)) [] qs



(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  map (fun tran -> (e_closure nfa (move nfa qs (Some tran)))) nfa.sigma
    
    

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold (fun a h -> let endStates = (e_closure nfa (move nfa qs (Some h))) in (qs, Some h, endStates)::a) [] nfa.sigma


let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if intersection qs nfa.fs = [] then [] else [qs]


let rec nfa_to_dfa_helper (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) (visited: 'q list list) : ('q list, 's) nfa_t =
  match work with
    [] -> dfa
  | ([])::t -> nfa_to_dfa_helper nfa dfa t visited
  | h::t ->
      if x_in_y h visited=true then nfa_to_dfa_helper nfa dfa t visited else
        let qs = h::dfa.qs in
        let finalStates = (new_finals nfa h)@dfa.fs in
        let delta = (new_trans nfa h)@dfa.delta in
        let newDfa = {sigma=dfa.sigma; qs=qs; delta=delta; q0=dfa.q0; fs=finalStates} in
        let newVisited = union visited [h] in
        let newWork = (new_states nfa h) @ t in
        nfa_to_dfa_helper nfa newDfa newWork newVisited

    



let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let empty_dfa = {qs= []; sigma= nfa.sigma; delta = []; q0=(e_closure nfa [nfa.q0]); fs= []} in
  nfa_to_dfa_helper nfa empty_dfa [(e_closure nfa [nfa.q0])] []



(* let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let empty_dfa = {qs= []; sigma= nfa.sigma; delta = []; q0=[nfa.q0]; fs= []} in
  nfa_to_dfa_helper nfa empty_dfa [(e_closure nfa [nfa.q0])] [] *)


let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"


(* let rec acceptHelper (nfa: ('q,char) nfa_t) charList currState =
  match charList with
  | [] -> 
      if (new_finals nfa currState) = [[]] then false else true
  | h::t ->
      if currState = [] then false else
        let newStates = (e_closure nfa (move nfa currState (Some h))) in
        acceptHelper nfa t newStates *)




let rec newStateTrans trans currState currTrans = fold (fun a (start, tran, ending) -> if tran = Some currTrans && (eq start currState) then ending else a) [] trans

let rec acceptHelper dfa charList currState = match charList with
  | [] -> if elem currState dfa.fs = true then true else false
  | h::t -> 
      let newState = newStateTrans dfa.delta currState h in
      if newState = [] then false else acceptHelper dfa t newState
          
          
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let charList = explode s in
  let dfa = nfa_to_dfa nfa in
  acceptHelper dfa charList dfa.q0


  
