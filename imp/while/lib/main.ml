open Ast
open Types
open Prettyprint

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


  (******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

let new_rho rho x v = fun x' -> if x' = x then v else rho x' 


let rec eval_expr (rho : state) e = match e with
  | True -> Bool true
  | False -> Bool false
  | Var(x) -> rho x
  | Const(n) -> Nat n
  | Not(e) -> (match eval_expr rho e with
    | Bool b -> Bool(not b)
    | x -> raise (TypeError ("Needs to be a boolean, instead it was " ^ string_of_val x))
  )

  | And(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Bool b1, Bool b2) -> Bool(b1 && b2)
    | _ -> raise (TypeError "Needs to be both booleans")  
  )

  | Or(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Bool b1, Bool b2) -> Bool(b1 || b2)
    | _ -> raise (TypeError "Needs to be both booleans")  
  )

  | Add(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Nat n1, Nat n2) -> Nat(n1 + n2)
    | _ -> raise (TypeError "Needs to be both naturals")  
  ) 

  | Sub(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Nat n1, Nat n2) -> Nat(n1 - n2)
    | _ -> raise (TypeError "Needs to be both naturals")  
  )

  | Mul(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Nat n1, Nat n2) -> Nat(n1 * n2)
    | _ -> raise (TypeError "Needs to be both naturals")  
  )

  | Eq(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Nat n1, Nat n2) -> Bool(n1 = n2)
    | _ -> raise (TypeError "Needs to be both naturals")  
  )

  | Leq(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Nat n1, Nat n2) -> Bool(n1 <= n2)
    | _ -> raise (TypeError "Needs to be both naturals")  
  )
;;

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

let new_state s x v = fun x' -> if x' = x then v else s x';;
let empty_state _ = raise (UnboundVar "value not defined");;
  
let rec trace1  = function
  | St(_) -> raise NoRuleApplies
  | Cmd(c, st) -> (match c with
    | Skip -> St st
    | Assign(x, e) -> St(new_state st x (eval_expr st e))
    | Seq(c1, c2) -> (match trace1 (Cmd(c1, st)) with
      | St(st') -> Cmd(c2, st')
      | Cmd(c1', st') -> Cmd(Seq(c1', c2), st')
    )
    | If(e, c1, c2) -> (match eval_expr st e with
      | Bool true -> Cmd(c1, st)
      | Bool false -> Cmd(c2, st)
      | _ -> raise (TypeError "If needs a boolean expression")
    )
    | While(e, c) -> (match eval_expr st e with
      | Bool true -> Cmd(Seq(c, While(e, c)), st)
      | Bool false -> St st
      | _ -> raise (TypeError ("condition on while needs to be a boolean, instead it was " ^ string_of_val (eval_expr st e)))
    )
  )

let rec trace_rec (n : int) (config : conf) =
  if n > 0 then  try 
    let config' = trace1 config in
    config::(trace_rec (n-1) config')
with NoRuleApplies -> [config]
  else [config]


let trace (n : int) (c : cmd) =  trace_rec n (Cmd(c, empty_state));;

