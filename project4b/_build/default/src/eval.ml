open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v


let negate boolean = match boolean with
  true -> false
  | false -> true
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
   let rec eval_expr env e = match e with
   | ID var -> eval_id_expr env var
   | Value value -> value
   | Not expr -> eval_not_expr env expr
   | Binop (op, expr1, expr2) -> eval_binop_expr op expr1 expr2 env
   | If (expr1, expr2, expr3) -> eval_if_expr expr1 expr2 expr3 env
   | Let (var, false, expr1, expr2) -> eval_non_rec_let_expr var expr1 expr2 env
   | Let (var, true, expr1, expr2) -> eval_rec_let_expr var expr1 expr2 env
   | Fun (var, expr1) -> eval_fun_expr var expr1 env
   | FunctionCall (expr1, expr2) -> eval_fun_call_expr expr1 expr2 env
 
 and eval_fun_call_expr expr1 expr2 env =
   let val1 = eval_expr env expr1 in
   match val1 with
   | Closure (envClosure, param, expr3) -> 
       let valExpr2 = eval_expr env expr2 in
       let newEnv = extend envClosure param valExpr2 in
       eval_expr newEnv expr3
         
   | _ -> raise (TypeError "bad bool")
 
 and eval_fun_expr var expr1 env =
   Closure (env, var, expr1)
  
 and eval_rec_let_expr var expr1 expr2 env =
   let env = extend_tmp env var in
   let initExprVal = eval_expr env expr1 in 
   let _ = (update env var initExprVal) in
   let bodyExprVal = eval_expr env expr2 in
   bodyExprVal
   
 and eval_non_rec_let_expr var expr1 expr2 env =
   let initExprVal = eval_expr env expr1 in
   let newEnv = extend env var initExprVal in
   let bodyExprVal = eval_expr newEnv expr2 in
   bodyExprVal
  
 and eval_id_expr env id =
   lookup env id
   
 and eval_not_expr env expr =
   let value = eval_expr env expr in
   match value with
   | Bool b -> Bool (negate b)
   | _ -> raise (TypeError "bad bool")
   
   and eval_binop_expr op expr1 expr2 env =
   let val1 = eval_expr env expr1 in
   let val2 = eval_expr env expr2 in
   match op, val1, val2 with
   | Add, Int int1, Int int2 -> Int (int1 + int2)
   | Sub, Int int1, Int int2 -> Int (int1 - int2) 
   | Mult, Int int1, Int int2 -> Int (int1 * int2)
   | Div, Int int1, Int 0 -> raise (DivByZeroError)
   | Div, Int int1, Int int2 -> Int (int1 / int2)
   | Greater, Int int1, Int int2 -> Bool (int1 > int2)
   | Less, Int int1, Int int2 -> Bool (int1 < int2)
   | LessEqual, Int int1, Int int2 -> Bool (int1 <= int2)
   | GreaterEqual, Int int1, Int int2 -> Bool (int1 >= int2)
   | Concat, String str1, String str2 -> String (str1 ^ str2)
   | Equal, Int var1, Int var2 -> Bool (var1 = var2)
   | Equal, Bool var1, Bool var2 -> Bool (var1 = var2)
   | Equal, String var1, String var2 -> Bool (var1 = var2)
   | NotEqual, Int var1, Int var2 -> Bool (var1 <> var2)
   | NotEqual, Bool var1, Bool var2 -> Bool (var1 <> var2)
   | NotEqual, String var1, String var2 -> Bool (var1 <> var2)
   | And, Bool var1, Bool var2 -> Bool (var1 && var2)
   | Or, Bool var1, Bool var2 -> Bool (var1 || var2)
   | _,_,_ -> raise (TypeError "bad not") 
   
 and eval_if_expr expr1 expr2 expr3 env =
   let val1 = eval_expr env expr1 in
   match val1 with
   | Bool true -> eval_expr env expr2
   | Bool false -> eval_expr env expr3
   | _ -> raise (TypeError "bad if")


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let rec eval_mutop env m = match m with
   | Def (var, expr1) -> eval_def_mutop var expr1 env
   | Expr expr -> eval_expr_mutop expr env
   | _ -> ([], None)
 
and eval_def_mutop var expr1 env =
   let env = extend_tmp env var in
   let initExprVal = eval_expr env expr1 in 
   let _ = (update env var initExprVal) in
   (env, Some initExprVal)
 
and eval_expr_mutop expr env =
   let val1 = eval_expr env expr in
   (env, Some val1)
