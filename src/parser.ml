open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let match_next toks comp = match toks with
|x::xs -> ((x = comp),xs)

and parse_id toks = match toks with
|Tok_ID(a)::rest -> a,rest
|_ -> raise (InvalidInputException "")

let rec parse_rec toks = match toks with
|Tok_Rec::rest -> true,rest
|a -> false,a
|_ -> raise (InvalidInputException "")

and parse_let toks =
  let rec_bool,remainder = parse_rec toks in
  let id,remainder = parse_id remainder in
  let _,remainder = match_next remainder Tok_Equal in
  let expr_1,remainder = parse_expr_helper remainder in
  let _,remainder = match_next remainder Tok_In in
  let expr_2,remainder = parse_expr_helper remainder in
  Let(id,rec_bool,expr_1,expr_2),remainder

and parse_func toks =
  let input,remainder = parse_id toks in
  let _,remainder = match_next remainder Tok_Arrow in
  let expr,remainder = parse_expr_helper remainder in
  Fun(input,expr),remainder

and parse_if toks =
  let expr_1,remainder = parse_expr_helper toks in
  let _,remainder = match_next remainder Tok_Then in
  let expr_2,remainder = parse_expr_helper remainder in
  let _,remainder = match_next remainder Tok_Else in
  let expr_3,remainder = parse_expr_helper remainder in
  If(expr_1,expr_2,expr_3),remainder

and parse_semi toks = match toks with
|Tok_Semi::rest -> true,rest
|a::rest -> false,(a::rest)
|_ -> raise (InvalidInputException "")

and parse_record_body toks =
  let name,remainder = parse_id toks in
  let _,remainder = match_next remainder Tok_Equal in
  let expr,remainder = parse_expr_helper remainder in
  let next,remainder = parse_semi remainder in
  if next then
    let xs,remainder = parse_record_body remainder in
    (Lab(name),expr)::xs,remainder
  else [(Lab(name),expr)],remainder

and parse_record toks = match toks with
|Tok_LCurly::Tok_RCurly::remainder -> Record([]),remainder
|Tok_LCurly::remainder ->
  let inside,remainder = parse_record_body remainder in
  let _,remainder = match_next remainder Tok_RCurly in
  (Record(inside),remainder)
|_ -> raise (InvalidInputException "")

and parse_primary toks = match toks with
|Tok_Int(x)::remainder -> Some(Int(x)),remainder
|Tok_Bool(x)::remainder -> Some(Bool(x)),remainder
|Tok_String(x)::remainder -> Some(String(x)),remainder
|Tok_ID(x)::remainder -> Some(ID(x)),remainder
|Tok_LParen::remainder ->
  let inside,remainder = parse_expr_helper remainder in
  let _,remainder = match_next remainder Tok_RParen in
  (Some(inside),remainder)
|Tok_LCurly::remainder -> let inside,remainder = parse_record (Tok_LCurly::remainder) in Some(inside),remainder
|_ -> None,toks

and parse_dot toks = match toks with
|Tok_Dot::remainder -> true,remainder
|a -> false,a
|_ -> raise (InvalidInputException "")

and parse_select toks = 
  let expr,remainder = parse_primary toks in
  let expr = match expr with |None -> raise (InvalidInputException "") |Some(a) -> a in
  let has_dot,remainder = parse_dot remainder in
  if has_dot then let name,remainder = parse_id remainder in Select(Lab(name),expr),remainder
  else expr,remainder

and parse_app toks = 
  let left,remainder = parse_select toks in
  let right,remainder = parse_primary remainder in
  match right with |None -> left,remainder |Some(right) -> App(left,right),remainder

and parse_unary toks = match toks with
|Tok_Not::remainder ->
  let right,remainder = parse_unary remainder in
  Not(right),remainder
|x -> parse_app x
|_ -> raise (InvalidInputException "")

and parse_concat toks = let left,remainder = parse_unary toks in
  match remainder with
  Tok_Concat::remainder ->
    let right,remainder = parse_concat remainder in
    Binop(Concat,left,right),remainder
  |_ -> (left,remainder)

and parse_multiplicative toks = let left,remainder = parse_concat toks in
  match remainder with
  |Tok_Mult::remainder ->
    let right,remainder = parse_multiplicative remainder in
    Binop(Mult,left,right),remainder
  |Tok_Div::remainder ->
    let right,remainder = parse_multiplicative remainder in
    Binop(Div,left,right),remainder
  |_ -> (left,remainder)

and parse_additive toks = let left,remainder = parse_multiplicative toks in
  match remainder with
  |Tok_Add::remainder ->
    let right,remainder = parse_additive remainder in
    Binop(Add,left,right),remainder
  |Tok_Sub::remainder ->
    let right,remainder = parse_additive remainder in
    Binop(Sub,left,right),remainder
  |_ -> (left,remainder)

and parse_relational toks = let left,remainder = parse_additive toks in
  match remainder with
  |Tok_Less::remainder -> 
    let right,remainder = parse_relational remainder in
    Binop(Less,left,right),remainder
  |Tok_Greater::remainder ->
    let right,remainder = parse_relational remainder in
    Binop(Greater,left,right),remainder
  |Tok_GreaterEqual::remainder ->
    let right,remainder = parse_relational remainder in
    Binop(GreaterEqual,left,right),remainder
  |Tok_LessEqual::remainder ->
    let right,remainder = parse_relational remainder in
    Binop(LessEqual,left,right),remainder
  |_ -> (left,remainder)

and parse_equality toks = let left,remainder = parse_relational toks in
  match remainder with
  |Tok_Equal::remainder ->
    let right,remainder = parse_equality remainder in
    Binop(Equal,left,right),remainder
  |Tok_NotEqual::remainder ->
    let right,remainder = parse_equality remainder in
    Binop(NotEqual,left,right),remainder
  |_ -> (left,remainder)

and parse_and toks = let left,remainder = parse_equality toks in
  match remainder with
  |Tok_And::remainder -> 
    let right,remainder = parse_and remainder in
    Binop(And,left,right),remainder
  |_ -> (left,remainder)

and parse_or toks = let left,remainder = parse_and toks in
  match remainder with
  |Tok_Or:: remainder ->
    let right,remainder = parse_or remainder in
    Binop(Or,left,right),remainder
  |_ -> (left,remainder)

and parse_expr_helper toks = match toks with
|Tok_Let::rest -> parse_let rest
|Tok_Fun::rest -> parse_func rest
|Tok_If::rest -> parse_if rest
|[] -> raise (InvalidInputException "")
|a -> parse_or a

let rec parse_expr toks =
  let expr,remainder = parse_expr_helper toks in
  remainder,expr


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = match toks with
|Tok_DoubleSemi::remainder -> [],NoOp
|Tok_Def::remainder ->
  let id,remainder = parse_id remainder in
  let _,remainder = match_next remainder Tok_Equal in
  let remainder,expr = parse_expr remainder in
  let _,remainder = match_next remainder Tok_DoubleSemi in
  remainder,Def(id,expr)
|_ -> 
  let remainder,expr = parse_expr toks in
  let _,remainder = match_next remainder Tok_DoubleSemi in
  remainder,Expr(expr)