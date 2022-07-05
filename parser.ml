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

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks = 
  let (rem_tok, expr) = parse_expr_work toks in 
  if rem_tok <> [] then raise (InvalidInputException "parse_expr") 
  else (rem_tok, expr)

and parse_expr_work toks = 
  let t = (lookahead toks) in 
  match t with 
  Some Tok_Let -> let (tok_after_let, expr) = parse_let (match_token toks Tok_Let) in (tok_after_let, expr)
  |Some Tok_If -> let (tok_after_if, expr) = parse_if (match_token toks Tok_If) in (tok_after_if, expr)
  |Some Tok_Fun -> let (tok_after_fun, expr) = parse_fun (match_token toks Tok_Fun) in (tok_after_fun, expr)
  |None -> raise (InvalidInputException "parse_expr")
  |_-> let (tok_after_or, expr) = parse_or toks in (tok_after_or, expr)


and get_id tok =
  match tok with 
  Some Tok_ID x -> x
  |_-> raise (InvalidInputException "get id")

and parse_let toks = 
  match (lookahead toks) with 
  Some Tok_Rec -> let is_rec = true in 
  let tok2 = match_token toks Tok_Rec in 
  let id = get_id (lookahead tok2) in let tok3 = match_token tok2 (Tok_ID id)
  in let tok4 = match_token tok3 Tok_Equal in 
  let (tok_after_expr, expr) = parse_expr_work tok4 in 
  let tok5 = match_token tok_after_expr Tok_In in 
  let (tok_after_expr2, expr2) = parse_expr_work tok5 in 
  (tok_after_expr2, Let (id, is_rec, expr, expr2))

  |_-> let is_rec = false in 
  let id = get_id (lookahead toks) in let tok2 = match_token toks (Tok_ID id)
  in let tok3 = match_token tok2 Tok_Equal in 
  let (tok_after_expr, expr) = parse_expr_work tok3 in 
  let tok4 = match_token tok_after_expr Tok_In in 
  let (tok_after_expr2, expr2) = parse_expr_work tok4 in 
  (tok_after_expr2, Let (id, is_rec, expr, expr2))
  

and parse_if toks = 
  let (tok_after_expr, expr) = parse_expr_work toks in 
  let tok2 = match_token tok_after_expr Tok_Then in 
  let (tok_after_expr2, expr2) = parse_expr_work tok2 in 
  let tok3 = match_token tok_after_expr2 Tok_Else in 
  let (tok_after_expr3, expr3) = parse_expr_work tok3 in
  (tok_after_expr3, If(expr, expr2, expr3))

and parse_fun toks = 
  match (lookahead toks) with 
  Some (Tok_ID x) -> let tok2 = match_token toks (Tok_ID x) in 
  let tok3 = match_token tok2 (Tok_Arrow) in let (tok_after_expr, expr) = parse_expr_work tok3 in 
  (tok_after_expr, Fun(x, expr))
  |_-> raise (InvalidInputException "parse def")


and parse_or toks = 
let (tok_after_and, expr) = parse_and toks in
match (lookahead tok_after_and) with 
Some Tok_Or -> let tok2 = (match_token tok_after_and Tok_Or) in 
let (tok_after_or, expr2) = parse_or tok2 in (tok_after_or, Binop(Or, expr, expr2))
|_-> (tok_after_and, expr)

and parse_and toks = 
let (tok_after_eq, expr1) = parse_eq toks in 
match (lookahead tok_after_eq) with 
  Some Tok_And -> let tok2 = match_token tok_after_eq Tok_And in 
  let (tok_after_and, expr2) = parse_and tok2 in 
  (tok_after_and, Binop(And, expr1, expr2))
  |_-> (tok_after_eq, expr1)

and tok_equal toks expr_o =
  let tok2 = match_token toks Tok_Equal in 
  let (tok_after_eq, expr) = parse_eq tok2  in 
  (tok_after_eq, Binop(Equal, expr_o, expr))

and tok_notequal toks expr_o =
  let tok2 = match_token toks Tok_NotEqual in 
  let (tok_after_eq, expr) = parse_eq tok2  in 
  (tok_after_eq, Binop(NotEqual, expr_o, expr))

and parse_eq toks = 
  let (tok_after_rel, expr) = parse_rel toks in 
  match (lookahead tok_after_rel) with
  Some Tok_Equal -> tok_equal tok_after_rel expr
  |Some Tok_NotEqual -> tok_notequal tok_after_rel expr
  |_-> (tok_after_rel, expr)

and tok_greater toks expr = 
let tok2 = match_token toks Tok_Greater in 
let (tok_after_rel, expr2) = parse_rel tok2 in
(tok_after_rel, Binop(Greater, expr, expr2))

and tok_less toks expr = 
let tok2 = match_token toks Tok_Less in 
let (tok_after_rel, expr2) = parse_rel tok2 in
(tok_after_rel, Binop(Less, expr, expr2))

and tok_greatereq toks expr = 
let tok2 = match_token toks Tok_GreaterEqual in 
let (tok_after_rel, expr2) = parse_rel tok2 in
(tok_after_rel, Binop(GreaterEqual, expr, expr2))

and tok_lesseq toks expr = 
let tok2 = match_token toks Tok_LessEqual in 
let (tok_after_rel, expr2) = parse_rel tok2 in
(tok_after_rel, Binop(LessEqual, expr, expr2))

and parse_rel toks = 
let (tok_after_add, expr) = parse_add toks in
  match (lookahead tok_after_add) with 
  Some Tok_Greater -> tok_greater tok_after_add expr
  |Some Tok_Less -> tok_less tok_after_add expr
  |Some Tok_GreaterEqual -> tok_greatereq tok_after_add expr
  |Some Tok_LessEqual -> tok_lesseq tok_after_add expr
  |_->(tok_after_add, expr)

and tok_add toks expr =
let tok2 = match_token toks Tok_Add in
let (tok_after_add, expr2) = parse_add tok2 in
(tok_after_add, Binop(Add, expr, expr2))

and tok_sub toks expr =
let tok2 = match_token toks Tok_Sub in
let (tok_after_add, expr2) = parse_add tok2 in
(tok_after_add, Binop(Sub, expr, expr2))

and parse_add toks = 
  let (tok_after_mul, expr) = parse_mul toks in
  match (lookahead tok_after_mul) with
  Some Tok_Add -> tok_add tok_after_mul expr
  |Some Tok_Sub -> tok_sub tok_after_mul expr
  |_-> (tok_after_mul, expr)

and tok_mul toks expr =
let tok2 = match_token toks Tok_Mult in
let (tok_after_mul, expr2) = parse_mul tok2 in 
(tok_after_mul, Binop(Mult, expr, expr2))

and tok_div toks expr =
let tok2 = match_token toks Tok_Div in
let (tok_after_mul, expr2) = parse_mul tok2 in 
(tok_after_mul, Binop(Div, expr, expr2))

and parse_mul toks = 
let (tok_after_concat, expr) = parse_concat toks in 
match (lookahead tok_after_concat) with 
Some Tok_Mult -> tok_mul tok_after_concat expr
|Some Tok_Div -> tok_div tok_after_concat expr
|_-> (tok_after_concat, expr)

and parse_concat toks = 
let (tok_after_una, expr) = parse_una toks in
match (lookahead tok_after_una) with 
Some Tok_Concat -> let tok2 = match_token tok_after_una Tok_Concat in 
let (tok_after_concat, expr2) = parse_concat tok2 in 
(tok_after_concat, Binop(Concat, expr, expr2))
|_-> (tok_after_una, expr)

and parse_una toks = 
let t = (lookahead toks) in 
match t with 
Some Tok_Not -> let tok2 = match_token toks Tok_Not in
let (tok_after_una, expr) = parse_una tok2 in 
(tok_after_una, Not(expr))
|_-> let (tok3, expr3 ) = parse_funcall toks in 
(tok3, expr3)


and parse_funcall toks = 
  let (tok_after_prime, expr) = parse_prime toks in 
  match (lookahead tok_after_prime) with 
  None -> (tok_after_prime, expr)
  |Some (Tok_Int x) -> let (tok2, expr2) = parse_prime tok_after_prime in (tok2, FunctionCall(expr, expr2))
  |Some (Tok_Bool x) -> let (tok2, expr2) = parse_prime tok_after_prime in (tok2, FunctionCall(expr, expr2))
  |Some (Tok_String x) -> let (tok2, expr2) = parse_prime tok_after_prime in (tok2, FunctionCall(expr, expr2))
  |Some (Tok_ID x) -> let (tok2, expr2) = parse_prime tok_after_prime  in (tok2, FunctionCall(expr, expr2))
  |Some Tok_LParen -> let (tok2, expr2) = parse_prime tok_after_prime in (tok2, FunctionCall(expr, expr2))
  |_-> (tok_after_prime, expr)


and parse_prime toks = 
  match (lookahead toks) with 
  Some (Tok_Int x) -> (match_token toks (Tok_Int x), Value (Int x))
  |Some (Tok_Bool x) -> let tok2 = match_token toks (Tok_Bool x) in (tok2, Value (Bool x))
  |Some (Tok_String x) -> (match_token toks (Tok_String x), Value (String x))
  |Some (Tok_ID x) -> (match_token toks (Tok_ID x), ID (x))
  |Some Tok_LParen -> let tok2 = match_token toks Tok_LParen in 
  let (tok_from_expr, expr) = parse_expr_work tok2 in 
  let tok4 = match_token tok_from_expr Tok_RParen in 
  (tok4, expr)
  |_-> raise (InvalidInputException "parse_prime")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
match lookahead toks with 
Some Tok_DoubleSemi -> ([], NoOp)
|_->
let (rem_tok, expr) = parse_mutop_work toks in
  (match_token rem_tok Tok_DoubleSemi, expr)

and parse_mutop_work toks = 
  match (lookahead toks) with
  Some Tok_Def -> let (tok_after_def, expr) = parse_def (match_token toks Tok_Def) in (tok_after_def, expr)
  |_-> let (tok_after_expr, expr) = parse_expr_work toks in (tok_after_expr, Expr expr)

and parse_def toks =
  let id = get_id (lookahead toks) in let toks = match_token toks (Tok_ID id) in 
  let toks = match_token toks Tok_Equal in let (tok_after_mtop, expr) = parse_mutop_work toks in 
  match expr with Expr expr -> (tok_after_mtop, (Def(id, expr)))
  |_-> raise (InvalidInputException "parse def")