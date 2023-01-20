open Ast

type exprval  = Bool of bool | Nat of int ;;

let string_of_val n = string_of_int n

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let bool_to_int (b : bool) : int = match b with
  | true -> 1
  | false -> 0

let int_to_bool (n : int) : bool = match n with
  | 0 -> false
  | _ -> true

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
exception PredOfZero

let rec is_nv = function
  | Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

  
let rec trace1 = function
  | True -> Succ(Zero)
  | False -> Zero

  | If(Succ(Zero),e1,_) -> e1
  | If(Zero,_,e2) -> e2
  | If(Succ(Succ(e0)), e1, e2) -> If(Succ(e0), e1, e2)
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)

  | Not(Succ(_)) -> Zero
  | Not(Zero) -> Succ(Zero)
  | Not(e) -> let e' = trace1 e in Not(e')

  | And(Succ(Succ(e1)), e2) -> And(Succ(e1), e2)
  | And(e1, Succ(Succ(e2))) -> And(e1, Succ(e2))
  | And(Succ(Zero),e) -> e
  | And(Zero,_) -> Zero
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)

  | Or(Succ(Succ(e1)), e2) -> Or(Succ(e1), e2)
  | Or(e1, Succ(Succ(e2))) -> Or(e1, Succ(e2))
  | Or(Succ(Zero),_) -> Succ(Zero)
  | Or(Zero,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)

  | Succ(e) -> let e' = trace1 e in Succ(e')

  | Pred(Zero) -> Zero
  | Pred(Succ(e)) -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')

  | IsZero(Zero) -> Succ(Zero)
  | IsZero(Succ(_)) -> Zero 
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies
;;

(*Changed to capture every type of Exception, also PredOfZero, kinda useless though*)
let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with _ -> [e]
;;

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

exception WrongType of string

let rec eval = function
    True -> 1
  | False -> 0
  | Zero -> 0

  | Not(e) ->
      let b = e |> eval |> int_to_bool in 
      bool_to_int (not b)
  
  | And(e1, e2) ->
      let b1 = e1 |> eval |> int_to_bool in
      let b2 = e2 |> eval |> int_to_bool in
      bool_to_int(b1 && b2)

  | Or(e1, e2) -> 
      let b1 = e1 |> eval |> int_to_bool in
      let b2 = e2 |> eval |> int_to_bool in
      bool_to_int(b1 || b2)

  | If(e0, e1, e2) ->
    let b0 = e0 |> eval |> int_to_bool in
    if b0 then eval e1 else eval e2

  | Succ(e) -> (eval e) + 1

  | Pred(e) -> (match eval e with
  | n when n = 0 -> 0
  | n when n > 0 -> n-1
  | _ -> raise (WrongType "Negative number, unexpected error")
  )

  |IsZero(e) -> bool_to_int(eval e = 0)
;;
