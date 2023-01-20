{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "0" { ZERO }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }    
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | eof { EOF }
