open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h))) 

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let getFirst tup = match tup with
    (a,b) -> a

let getSecond tup = match tup with
    (a,b) -> b


let productionRecExpr toks =
  let isRec = lookahead toks = Some Tok_Rec in
  let newToks = if isRec then match_token toks Tok_Rec else toks in
  (newToks, isRec)


let productiontokIDExpr toks = 
  let nextToken = lookahead toks in
  match nextToken with
    Some Tok_ID id -> ((match_token toks (Tok_ID id)), id)
  | _ -> raise (InvalidInputException "sorryb")


let productionXExpr toks x =
  let nextToken = lookahead toks in
  match nextToken with
    Some x -> match_token toks x
  | _ -> raise (InvalidInputException "sorryc")

and isNextPrimary toks =
  let nextToken = lookahead toks in
  match nextToken with
  | Some Tok_Int intVar -> 
      true
  | Some Tok_Bool boolVar -> 
      true
  | Some Tok_String stringVar -> 
      true
  | Some Tok_ID id -> 
      true
  | Some Tok_LParen -> 
      true
  | _ -> false 
  
    
let rec productionPrimaryExpression toks =
  let nextToken = lookahead toks in
  match nextToken with
  | Some Tok_Int intVar -> 
      let toks = match_token toks (Tok_Int intVar) in
      (toks, Value (Int intVar))
  | Some Tok_Bool boolVar -> 
      let toks = match_token toks (Tok_Bool boolVar) in
      (toks, Value (Bool boolVar))
  | Some Tok_String stringVar -> 
      let toks = match_token toks (Tok_String stringVar) in
      (toks, Value (String stringVar))
  | Some Tok_ID id -> 
      let toks = match_token toks (Tok_ID id) in
      (toks, ID id)
  | Some Tok_LParen -> 
      let toks = match_token toks Tok_LParen in
      let exprExpr1 = productionExpr toks in
      let toks = getFirst exprExpr1 in
      let expr1 = getSecond exprExpr1 in
      let toks = productionXExpr toks Tok_RParen in
      (toks, expr1)
  | Some Tok_RParen -> raise (InvalidInputException "sorrya") 
  | _ -> raise (InvalidInputException "sorryd") 
and productionFunctionCallExpression toks =
  let exprExpr1 = productionPrimaryExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let anothorPrimary = isNextPrimary toks in
  if anothorPrimary then
    let exprExpr2 = productionPrimaryExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    (toks, FunctionCall (expr1, expr2))
  else
    exprExpr1 
and productionFunctionExpression toks =
  let toks = productionXExpr toks Tok_Fun in
  let tokIDExpr = productiontokIDExpr toks in
  let toks = getFirst tokIDExpr in
  let id = getSecond tokIDExpr in
  let toks = productionXExpr toks Tok_Arrow in
  let exprExpr1 = productionExpr toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  (toks, Fun (id, expr1)) 
and productionUnaryExpression toks =
  let nextToken = lookahead toks in
  
  if nextToken = Some Tok_Not then
    let toks = productionXExpr toks Tok_Not in
    let exprExpr1 = productionUnaryExpression toks in
    let toks = getFirst exprExpr1 in
    let expr1 = getSecond exprExpr1 in
    (toks, Not expr1)
  else
    productionFunctionCallExpression toks 
and productionConcatExpression toks =
  let exprExpr1 = productionUnaryExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let nextToken = lookahead toks in
  if nextToken = Some Tok_Concat then
    let toks = productionXExpr toks Tok_Concat in
    let exprExpr2 = productionConcatExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    (toks, Binop (Concat, expr1, expr2))
  else exprExpr1 
and productionMultiplicativeExpression toks =
  let exprExpr1 = productionConcatExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let nextToken = lookahead toks in
  if nextToken = Some Tok_Div || nextToken = Some Tok_Mult then
    let toks = productionXExpr toks nextToken in
    let exprExpr2 = productionMultiplicativeExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    if nextToken = Some Tok_Mult then
      (toks, Binop (Mult, expr1, expr2))
    else(toks, Binop (Div, expr1, expr2))
  else
    exprExpr1 
and productionAdditiveExpression toks =
  let exprExpr1 = productionMultiplicativeExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let nextToken = lookahead toks in
  if nextToken = Some Tok_Add || nextToken = Some Tok_Sub then
    let toks = productionXExpr toks nextToken in
    let exprExpr2 = productionAdditiveExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    if nextToken = Some Tok_Add then
      (toks, Binop (Add, expr1, expr2))
    else (toks, Binop (Sub, expr1, expr2))
  else
    exprExpr1 
and productionRelationalExpression toks =
  let exprExpr1 = productionAdditiveExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let nextToken = lookahead toks in
  if nextToken = Some Tok_Less || nextToken = Some Tok_Greater || nextToken = Some Tok_LessEqual || nextToken = Some Tok_GreaterEqual then
    let toks = productionXExpr toks nextToken in
    let exprExpr2 = productionRelationalExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    if nextToken = Some Tok_Less then
      (toks, Binop (Less, expr1, expr2))
    else if nextToken = Some Tok_Greater then
      (toks, Binop (Greater, expr1, expr2))
    else if nextToken = Some Tok_GreaterEqual then
      (toks, Binop (GreaterEqual, expr1, expr2))
    else
      (toks, Binop (LessEqual, expr1, expr2)) 
  else
    exprExpr1 
and productionEqualityExpression toks =
  let exprExpr1 = productionRelationalExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let nextToken = lookahead toks in
  if nextToken = Some Tok_Equal || nextToken = Some Tok_NotEqual then
    let toks = productionXExpr toks nextToken in
    let exprExpr2 = productionEqualityExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    if nextToken = Some Tok_Equal then
      (toks, Binop (Equal, expr1, expr2))
    else (toks, Binop (NotEqual, expr1, expr2))
  else
    exprExpr1 
and productionAndExpression toks =
  let exprExpr1 = productionEqualityExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let nextToken = lookahead toks in
  if nextToken = Some Tok_And then
    let toks = productionXExpr toks nextToken in
    let exprExpr2 = productionAndExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    (toks, Binop (And, expr1, expr2))
  else
    exprExpr1 
and productionOrExpression toks =
  let exprExpr1 = productionAndExpression toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let nextToken = lookahead toks in
  if nextToken = Some Tok_Or then
    let toks = productionXExpr toks nextToken in
    let exprExpr2 = productionOrExpression toks in
    let toks = getFirst exprExpr2 in
    let expr2 = getSecond exprExpr2 in
    (toks, Binop (Or, expr1, expr2))
  else
    exprExpr1 
and productionLetExpr toks =
  let toks = match_token toks Tok_Let in
  let recursionExpr = productionRecExpr toks in
  let toks = getFirst recursionExpr in
  let isRec = getSecond recursionExpr in
  let tokIDExpr = productiontokIDExpr toks in
  let toks = getFirst tokIDExpr in
  let id = getSecond tokIDExpr in
  let toks = productionXExpr toks Tok_Equal in
  let exprExpr1 = productionExpr toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let toks = productionXExpr toks Tok_In in
  let exprExpr2 = productionExpr toks in
  let toks = getFirst exprExpr2 in
  let expr2 = getSecond exprExpr2 in
  (toks, Let (id, isRec, expr1, expr2)) 
and productionIfExpression toks =
  let toks = match_token toks Tok_If in
  let exprExpr1 = productionExpr toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let toks = productionXExpr toks Tok_Then in
  let exprExpr2 = productionExpr toks in
  let toks = getFirst exprExpr2 in
  let expr2 = getSecond exprExpr2 in
  let toks = productionXExpr toks Tok_Else in
  let exprExpr3 = productionExpr toks in
  let toks = getFirst exprExpr3 in
  let expr3 = getSecond exprExpr3 in
  (toks, If (expr1, expr2, expr3)) 
and productionExpr toks = 
  let nextToken = lookahead toks in
  if nextToken = Some (Tok_Let) then
    let exprExpr1 = productionLetExpr toks in
    exprExpr1
  else if nextToken = Some (Tok_If) then
    let exprExpr1 = productionIfExpression toks in
    exprExpr1
  else if nextToken = Some (Tok_Fun) then
    let exprExpr1 = productionFunctionExpression toks in
    exprExpr1
  else productionOrExpression toks


let rec parse_expr toks = productionExpr toks

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  let nextToken = lookahead toks in
  if nextToken = Some Tok_Def then
    parse_def_mutop toks
  else if nextToken = Some Tok_DoubleSemi then
    let toks = productionXExpr toks Tok_DoubleSemi in
    (toks, NoOp)
  else parse_expr_mutop toks


and parse_def_mutop toks =
  let toks = productionXExpr toks Tok_Def in
  let tokIDExpr = productiontokIDExpr toks in
  let toks = getFirst tokIDExpr in
  let id = getSecond tokIDExpr in
  let toks = productionXExpr toks Tok_Equal in
  let exprExpr1 = productionExpr toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let toks = productionXExpr toks Tok_DoubleSemi in
  (toks, Def (id, expr1))

and parse_expr_mutop toks = 
  let exprExpr1 = productionExpr toks in
  let toks = getFirst exprExpr1 in
  let expr1 = getSecond exprExpr1 in
  let toks = productionXExpr toks Tok_DoubleSemi in
  (toks, Expr expr1)

