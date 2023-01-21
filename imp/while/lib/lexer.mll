{
open Parser
}

let white = [' ' '\n' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let alph = ['a' - 'z' 'A' - 'Z' '0' - '9']
let id = letter alph*

(*This way a number can be a 1 digit or multiple digits, to do so it can not start with 0*)
let num = ['0' - '9'] | ['1' - '9']['0' - '9']*

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
