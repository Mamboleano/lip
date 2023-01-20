open Ast

type exprval  = Bool of bool | Nat of int ;;

let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n

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
  | Var(x) -> x
  | Let(x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

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
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Zero) -> raise PredOfZero
  | Pred(Succ(nv))  when is_nv nv -> nv
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(nv))  when is_nv nv -> False 
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
exception UnboundValue

let new_rho rho x v = fun x' -> if x' = x then v else rho x' 


let rec eval' e rho = match e with
    True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | Not(e) -> (match eval' e  rho with
    | Bool b -> Bool(not b)
    | _ -> raise (WrongType "Needs to be a boolean")
  )
  | And(e1, e2) -> (match (eval' e1 rho , eval' e2 rho ) with
    | (Bool b1, Bool b2) -> Bool(b1 && b2)
    | _ -> raise (WrongType "Needs to be both booleans")  
  )
  | Or(e1, e2) -> (match (eval' e1 rho , eval' e2 rho ) with
    | (Bool b1, Bool b2) -> Bool(b1 || b2)
    | _ -> raise (WrongType "Needs to be both booleans")  
  )
  | If(e0, e1, e2) -> (match eval' e0 rho with
    | Bool b -> if b then eval' e1 rho else eval' e2 rho
    | _ -> raise (WrongType "Condition needs to be a boolean")
  )

  | Succ(e) -> (match eval' e rho with
    | Nat n -> Nat (n+1)
    | _ -> raise (WrongType "Needs to be a natural")
    )

  | Pred(e) -> (match eval' e rho with
  | Nat n  when n > 0 -> Nat (n-1)
  | Nat n when n = 0-> raise PredOfZero
  | _ -> raise (WrongType "Needs to be a natural")
  )

  |IsZero(e) -> (match eval' e rho with
    | Nat n -> Bool(n=0)
    | _ -> raise (WrongType"needs to be a natural")
  )

  | Var(x) -> rho x

  | Let(x, e1, e2) -> let v1 = eval' e1 rho in eval' e2 (new_rho rho x v1)
;;

let eval e = eval' e (fun _ -> raise UnboundValue) 

