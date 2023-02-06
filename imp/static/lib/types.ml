open Ast
    
type loc = int

type memval = int
type mem = loc -> memval

(* The second component of the IFun constructor holds the environment at the
   time of declaration of a function *)
type env = ide -> envval
and envval = IVar of loc | IFun of ide * env * cmd * expr

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type state = env list * mem * loc

let topenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_) = el
let getmem (_,m,_) = m
let getloc (_,_,l) = l
  
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of ide
