open Ast
open Types
open Prettyprint

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(*To get the value of a variable x, first we get the top environment from the stack
   then we fetch the location of our variable x, and then we find the value stored
   in memory for that location
*)
let get_val (st : state) (x : ide) : memval = match (topenv st) x with
  | BVar l
  | IVar l -> (getmem st) l

  (******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

let new_rho rho x v = fun x' -> if x' = x then v else rho x' 


let rec eval_expr (rho : state) e = match e with
  | True -> Bool true
  | False -> Bool false
  | Var(x) -> get_val rho x
  | Const(n) -> Int n
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
    | (Int n1, Int n2) -> Int(n1 + n2)
    | _ -> raise (TypeError "Needs to be both Inturals")  
  ) 

  | Sub(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Int n1, Int n2) -> Int(n1 - n2)
    | _ -> raise (TypeError "Needs to be both Inturals")  
  )

  | Mul(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Int n1, Int n2) -> Int(n1 * n2)
    | _ -> raise (TypeError "Needs to be both Inturals")  
  )

  | Eq(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Int n1, Int n2) -> Bool(n1 = n2)
    | _ -> raise (TypeError "Needs to be both Inturals")  
  )

  | Leq(e1, e2) -> (match (eval_expr rho e1 , eval_expr rho e2 ) with
    | (Int n1, Int n2) -> Bool(n1 <= n2)
    | _ -> raise (TypeError "Needs to be both Inturals")  
  )
;;

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

let bind_env f x v = fun y -> if y=x then v else f y

let bind_mem f x v = fun y -> if y=x then v else f y

let empty_env x = raise (UnboundVar ("variable " ^ x ^ " not defined"));;
let empty_mem l = raise (UnboundVar ("memory location " ^ string_of_int l ^ " not allocated"));;

let rec add_decl (d : decl) (e : env) (l : loc) = match d with
  | EmptyDecl -> (e, l)
  | IntVar (x , d') ->  add_decl d' (bind_env e x (IVar l)) (l+1)
  | BoolVar (x , d') -> add_decl d' (bind_env e x (BVar l)) (l+1)


  
let rec trace1  = function
  | St(_) -> raise NoRuleApplies
  | Cmd(c, st) -> (match c with
    | Skip -> St st
    | Assign(x, e) -> (match (topenv st x , eval_expr st e) with
      | (BVar l, Bool b) -> St (getenv st, bind_mem (getmem st) l (Bool b), getloc st)
      | (IVar l, Int n) -> St (getenv st, bind_mem (getmem st) l (Int n), getloc st)
      | _ -> raise (TypeError "The type of the value assigned and the type of the variable must coincide")
    )
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
    | Decl(d, c) -> 
        let (new_env, new_l) = add_decl d (topenv st) (getloc st) in
        (*We need to enclose the cmd to execute into a block, otherwise new declarations
           could overwrite some variables already defined*)
        trace1(Cmd(Block(c), (new_env::(getenv st), getmem st, new_l)))
    | Block(c) -> match trace1(Cmd(c, st)) with
      | St st' -> St (popenv st', getmem st', getloc st')
      | Cmd(c', st') -> Cmd(Block(c'), st')
  )

let rec trace_rec (n : int) (config : conf) =
  if n > 0 then  try 
    let config' = trace1 config in
    config::(trace_rec (n-1) config')
with NoRuleApplies -> [config]
  else [config]


let trace (n : int) (c : cmd) =  trace_rec n (Cmd(c, ([empty_env], empty_mem, 0)));;

