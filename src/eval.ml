open Types
open Lexer
open Parser
(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)

let rec eval_id env id = lookup env id

and eval_not env a = match (eval_expr_helper env a) with
|Bool(a) -> Bool(not a)
|_ -> raise (TypeError "not a boolean")

and eval_int_pair env a b =
  let a = match (eval_expr_helper env a) with |Int(a) -> a | _ -> raise (TypeError "a is not an int") in
  let b = match (eval_expr_helper env b) with |Int(b) -> b | _ -> raise (TypeError "b is not an int") in
  a,b

and eval_add env a b =
  let a,b = eval_int_pair env a b in
  Int(a + b)

and eval_sub env a b =
  let a,b = eval_int_pair env a b in
  Int(a - b)

and eval_mult env a b =
  let a,b = eval_int_pair env a b in
  Int(a * b)

and eval_div env a b =
  let a,b = eval_int_pair env a b in
  if b = 0 then raise (DivByZeroError) else Int(a / b)

and eval_greater env a b =
  let a,b = eval_int_pair env a b in
  Bool(a > b)

and eval_less env a b =
  let a,b = eval_int_pair env a b in
  Bool(a < b)

and eval_greater_equal env a b =
  let a,b = eval_int_pair env a b in
  Bool(a >= b)

and eval_less_equal env a b =
  let a,b = eval_int_pair env a b in
  Bool(a <= b)

and eval_concat env a b =
  let a = match (eval_expr_helper env a) with |String(a) -> a | _ -> raise (TypeError "a is not a string") in
  let b = match (eval_expr_helper env b) with |String(b) -> b | _ -> raise (TypeError "b is not a string") in
  String(a ^ b)

and eval_equal env a b =
  let a = eval_expr_helper env a in
  let b = eval_expr_helper env b in
  match a,b with
  |Int(a),Int(b) -> Bool(a = b)
  |Bool(a),Bool(b) -> Bool(a = b)
  |String(a),String(b) -> Bool(a = b)
  |Closure(_),Closure(_) -> raise (TypeError "comparing closures")
  | _ -> raise (TypeError "types don't match")

and eval_not_equal env a b =
  let a = eval_expr_helper env a in
  let b = eval_expr_helper env b in
  match a,b with
  |Int(a),Int(b) -> Bool(a <> b)
  |Bool(a),Bool(b) -> Bool(a <> b)
  |String(a),String(b) -> Bool(a <> b)
  |Closure(_),Closure(_) -> raise (TypeError "comparing closures")
  | _ -> raise (TypeError "types don't match")

and eval_or env a b =
  let a = match (eval_expr_helper env a) with |Bool(a) -> a | _ -> raise (TypeError "a is not a bool") in
  let b = match (eval_expr_helper env b) with |Bool(b) -> b | _ -> raise (TypeError "b is not a bool") in
  Bool(a || b)

and eval_and env a b =
  let a = match (eval_expr_helper env a) with |Bool(a) -> a | _ -> raise (TypeError "a is not a bool") in
  let b = match (eval_expr_helper env b) with |Bool(b) -> b | _ -> raise (TypeError "b is not a bool") in
  Bool(a && b)

and eval_binop env expr a b = match expr with
|Add -> eval_add env a b
|Sub -> eval_sub env a b
|Mult -> eval_mult env a b
|Div -> eval_div env a b
|Greater -> eval_greater env a b
|Less -> eval_less env a b
|GreaterEqual -> eval_greater_equal env a b
|LessEqual -> eval_less_equal env a b
|Concat -> eval_concat env a b
|Equal -> eval_equal env a b
|NotEqual -> eval_not_equal env a b
|Or -> eval_or env a b
|And -> eval_and env a b

and eval_if env guard t f =
  let guard = match eval_expr_helper env guard with |Bool(guard) -> guard | _ -> raise (TypeError "guard is not a bool") in
  if guard then eval_expr_helper env t else eval_expr_helper env f

and eval_let env name rec_bool init body =
  if rec_bool then
    let env = extend_tmp env name in
    let init = eval_expr_helper env init in
    let _ = update env name init in
    eval_expr_helper env body
  else
    let init = eval_expr_helper env init in
    let env = extend env name init in
    eval_expr_helper env body

and eval_fun env name expr = Closure(env,name,expr)

and eval_app env func input =
  let a,x,e = match eval_expr_helper env func with |Closure(a,x,e) -> a,x,e | _ -> raise (TypeError "should be a closure") in
  let v = eval_expr_helper env input in
  let a = extend a x v in
  eval_expr_helper a e

and eval_select env name expr =
  let record_contents = match eval_expr_helper env expr with |Record(contents) -> contents | _ -> raise (TypeError "should be a record") in
  let rec search_record xs = match xs with
    |[] -> raise (SelectError "couldn't find label")
    |(label,contents)::xs -> if label = name then contents else search_record xs in
  search_record record_contents

and eval_expr_helper env e = match e with
|ID(a) -> eval_id env a
|Not(a) -> eval_not env a
|Binop(expr,a,b) -> eval_binop env expr a b
|If(guard,t,f) -> eval_if env guard t f
|Let(name,rec_bool,init,body) -> eval_let env name rec_bool init body
|Fun(name,expr) -> eval_fun env name expr
|App(func,input) -> eval_app env func input
|Select(name,expr) -> eval_select env name expr
|_ -> e

let rec eval_expr env e = eval_expr_helper env e

let rec expr input =
  let tokens = tokenize input in
  let _,expr = parse_expr tokens in
  eval_expr [] expr

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
|Def(name,expr) -> let env = extend_tmp env name in let expr = eval_expr env expr in let _ = update env name expr in env,Some(expr)
|NoOp -> [],None
|Expr(a) -> env,Some(eval_expr env a)
