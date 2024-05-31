{
  type token =
    | EOF
    | LPAREN
    | RPAREN
    | COMMA
    | DOT
    | IF
    | THEN
    | ELSE
    | ID of string
    | INT of int
    | VAR of string
    | FUN of string
}

let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule tokenize = parse
  | [' ' '\t' '\r' '\n']        { tokenize lexbuf }   
  | '('                          { LPAREN }
  | ')'                          { RPAREN }
  | ','                          { COMMA }
  | '.'                          { DOT }
  | "if"                         { IF }
  | "then"                       { THEN }
  | "else"                       { ELSE }
  | identifier as id             { ID id }
  | ['0'-'9']+ as num            { INT (int_of_string num) }
  | _ as var                     { VAR (Lexing.lexeme lexbuf) }
  | eof                          { EOF }
    