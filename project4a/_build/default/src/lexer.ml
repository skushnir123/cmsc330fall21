






open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec fold f a lst = 
  match lst with 
  | [] -> a
  | h::t -> fold f (f a h) t

let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let rec elem x a =
  match a with
  | h::t -> (h = x) || (elem x t)
  | [] -> false

let rec insert x a =
  if not (elem x a) then x::a else a

let insert_all (xs : 'a list) (a : 'a list) : 'a list =
  List.fold_right insert xs a

let rec subset a b =
  match a with
  | h::t -> (elem h b) && (subset t b)
  | [] -> true

let rec eq a b = (subset a b) && (subset b a)

let rec reverse arr = match arr with 
[] -> []
| h::t -> (reverse t)@[h]

let rec charArrayToString arr = match arr with
[] -> ""
| h::t -> 
  let stringChar = String.make 1 h in
  stringChar ^ (charArrayToString t)

let rec tokenizeHelper readingString currentString acc inQuotes = match readingString with
    [] -> if (eq currentString []) then acc else currentString::acc
  | h::t -> if inQuotes = true then
    if h = '\"' then tokenizeHelper t (currentString@[h]) acc false
    else tokenizeHelper t (currentString@[h]) acc true
  else if h = '\"' then tokenizeHelper t (currentString@[h]) acc true
  else if h = ' '  then
        if (eq currentString []) then tokenizeHelper t currentString acc inQuotes else tokenizeHelper t [] (currentString::acc) inQuotes
      else tokenizeHelper t (currentString@[h]) acc inQuotes

let tokensList str = reverse (tokenizeHelper (explode str) [] [] false)

let regexList = [(Str.regexp "\\([0-9]+\\)+\\|(\\(-[0-9]+\\))",fun matchedGroup -> Tok_Int (int_of_string matchedGroup));
                (Str.regexp "\\(fun[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(fun\\)",fun matchedGroup -> Tok_Fun);
                (Str.regexp "\\(not[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(not\\)",fun matchedGroup -> Tok_Not);
                (Str.regexp "\\(if[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(if\\)",fun matchedGroup -> Tok_If);
                (Str.regexp "\\(else[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(else\\)",fun matchedGroup -> Tok_Else);
                (Str.regexp "\\(then[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(then\\)",fun matchedGroup -> Tok_Then);
                (Str.regexp "\\(let[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(let\\)",fun matchedGroup -> Tok_Let);
                (Str.regexp "\\(def[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(def\\)",fun matchedGroup -> Tok_Def);
                (Str.regexp "\\(rec[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(rec\\)",fun matchedGroup -> Tok_Rec);
                (Str.regexp "\\(in[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(in\\)",fun matchedGroup -> Tok_In);
                (Str.regexp "\\(true[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(true\\)",fun matchedGroup -> Tok_Bool true);
                (Str.regexp "\\(false[a-zA-Z0-9]+\\)",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\\(false\\)",fun matchedGroup -> Tok_Bool false);
                (Str.regexp "\\([a-zA-Z][a-zA-Z0-9]*\\)+",fun matchedGroup -> Tok_ID (matchedGroup));
                (Str.regexp "\"\\([^\"]*\\)\"",fun matchedGroup -> Tok_String (matchedGroup));
                (Str.regexp "\\((\\)",fun matchedGroup -> Tok_LParen);
                (Str.regexp "\\()\\)",fun matchedGroup -> Tok_RParen);
                (Str.regexp "\\(=\\)",fun matchedGroup -> Tok_Equal);
                (Str.regexp "\\(<>\\)",fun matchedGroup -> Tok_NotEqual);
                (Str.regexp "\\(>=\\)",fun matchedGroup -> Tok_GreaterEqual);
                (Str.regexp "\\(<=\\)",fun matchedGroup -> Tok_LessEqual);
                (Str.regexp "\\(||\\)",fun matchedGroup -> Tok_Or);
                (Str.regexp "\\(&&\\)",fun matchedGroup -> Tok_And); 
                (Str.regexp "\\(->\\)",fun matchedGroup -> Tok_Arrow);
                (Str.regexp "\\(>\\)",fun matchedGroup -> Tok_Greater);
                (Str.regexp "\\(<\\)",fun matchedGroup -> Tok_Less);
                (Str.regexp "\\(;;\\)",fun matchedGroup -> Tok_DoubleSemi);
                (Str.regexp "\\(+\\)",fun matchedGroup -> Tok_Add);
                (Str.regexp "\\(-\\)",fun matchedGroup -> Tok_Sub);
                (Str.regexp "\\(*\\)",fun matchedGroup -> Tok_Mult);
                (Str.regexp "\\(/\\)",fun matchedGroup -> Tok_Div);
                (Str.regexp "\\(\\^\\)",fun matchedGroup -> Tok_Concat);
] 

let rec findMatchingRegex regList str = match regList with
| [] -> (fun matchedGroup -> Tok_Int 613)
| (h,func)::t -> if Str.string_match h str 0 then func else findMatchingRegex t str


let rec convertString str = 
    let strLength = String.length str in
    if strLength = 0 then [] else
    let matchingRegex = (findMatchingRegex regexList str) in
    if (matchingRegex "8") = Tok_Int 613 then raise (InvalidInputException "sorry") else
    let indexOfEnd = (Str.match_end ()) in
    let newLength = strLength-indexOfEnd in
    let newSubstring = String.sub str indexOfEnd newLength in
    let matchedGroup = 
      try
      (Str.matched_group 1 str) with
      Not_found -> (Str.matched_group 2 str) 
     in
    (matchingRegex matchedGroup)::(convertString newSubstring)

let tokenize input = 
  let charList = tokensList input in
  let stringList = List.map (charArrayToString) charList in
  let tokens = (fold (fun a h -> a@(convertString h)) [] stringList) in
  tokens
