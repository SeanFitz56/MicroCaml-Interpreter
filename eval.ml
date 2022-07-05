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
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  Value v -> v
  |ID var -> eval_id env var
  |Fun (var, expr) -> eval_fun env var expr
  |Not expr -> eval_not env expr
  |Binop (op, expr1, expr2) -> eval_binop env op expr1 expr2
  |If (expr1, expr2, expr3) -> eval_if env expr1 expr2 expr3
  |FunctionCall (expr1, expr2) -> eval_funcall env expr1 expr2
  |Let (var, bool, expr1, expr2) -> eval_let env var bool expr1 expr2


and eval_id env var = 
  match lookup env var with 
  Int x -> Int x
  |Bool bool -> Bool bool
  |String str -> String str
  |Closure (env, var, expr) -> Closure (env, var, expr)

and eval_fun env var expr = 
  Closure (env, var, expr)

and eval_not env e = 
  match e with 
  Value (Bool bool) -> Bool (not bool)
  |Value _-> raise (TypeError "eval_not")
  |ID x -> negate_id env x
  |_-> negate (eval_expr env e)

and negate_id env x = 
  match lookup env x with 
  Bool bool -> Bool (not bool)
  |_-> raise (TypeError "negate id")

and negate v = 
  match v with 
  Bool b -> Bool (not b)
  |_-> raise (TypeError "negate")

and eval_binop env op expr1 expr2 = 
  match op with 
  Add -> eval_add env expr1 expr2
  |Sub -> eval_sub env expr1 expr2
  |Mult -> eval_mult env expr1 expr2
  |Div -> eval_div env expr1 expr2 
  |Equal -> eval_eq env expr1 expr2
  |NotEqual -> eval_noteq env expr1 expr2
  |GreaterEqual -> eval_greatereq env expr1 expr2
  |LessEqual -> eval_lesseq env expr1 expr2
  |Greater -> eval_greater env expr1 expr2
  |Less -> eval_less env expr1 expr2
  |Or -> eval_or env expr1 expr2
  |And -> eval_and env expr1 expr2
  |Concat -> eval_concat env expr1 expr2

and eval_add env expr1 expr2 = 
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Int (x + y)
  |_-> raise (TypeError "eval_add"))
  |_-> raise (TypeError "eval_add")

and eval_sub env expr1 expr2 = 
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Int (x - y)
  |_-> raise (TypeError "eval_sub"))
  |_-> raise (TypeError "eval_sub")

and eval_mult env expr1 expr2 = 
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Int (x * y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_div env expr1 expr2 = 
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> if y = 0 then raise DivByZeroError else Int (x / y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_eq env expr1 expr2 =
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Bool (x = y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_noteq env expr1 expr2 =
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Bool (not(x = y))
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_greatereq env expr1 expr2 =
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Bool (x >= y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_lesseq env expr1 expr2 =
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Bool (x <= y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_greater env expr1 expr2 =
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Bool (x > y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_less env expr1 expr2 =
  match (eval_expr env expr1) with 
  Int x -> (match (eval_expr env expr2) with 
  Int y -> Bool (x < y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_or env expr1 expr2 =
  match (eval_expr env expr1) with 
  Bool x -> (match (eval_expr env expr2) with 
  Bool y -> Bool (x||y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_and env expr1 expr2 =
  match (eval_expr env expr1) with 
  Bool x -> (match (eval_expr env expr2) with 
  Bool y -> Bool (x&&y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_concat env expr1 expr2 =
  match (eval_expr env expr1) with 
  String x -> (match (eval_expr env expr2) with 
  String y -> String (x^y)
  |_-> raise (TypeError "eval_mult"))
  |_-> raise (TypeError "eval_mult")

and eval_if env expr texpr fexpr = 
  match (eval_expr env expr) with 
  Bool x -> if x then
  eval_expr env texpr else eval_expr env fexpr
  |_-> raise (TypeError "eval_if")

and eval_funcall env expr1 expr2 = 
  match (eval_expr env expr1) with 
  (Closure (a, x, expr)) -> let v2 = (eval_expr env expr2) in eval_expr (extend a x v2) expr
  |_-> raise (TypeError "eval_funcall")

and eval_let env var bool expr1 expr2 =
  if not bool then 
  eval_expr (extend env var (eval_expr env expr1)) expr2 
  else 
  let env_tmp = (extend_tmp env var) in let v = eval_expr env_tmp expr1 in let () = update env_tmp var v in (eval_expr env_tmp expr2)
 
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  NoOp -> (env, None)
  |Expr expr -> (env, Some (eval_expr env expr))
  |Def (var, expr) -> let nenv = (extend_tmp env var) in let value = eval_expr nenv expr in let () = update nenv var value in (nenv, Some value)