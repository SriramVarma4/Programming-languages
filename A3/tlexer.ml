open Printf
open Lexer

let rec token_string = function
  | EOF -> "EOF"
  | PLUS -> "ARTH_OP PLUS: +"
  | MINUS -> "ARTH_OP MINUS: -"
  | TIMES -> "ARTH_OP TIMES: *"
  | DIVIDE -> "ARTH_OP DIVIDE: /"
  | MODULO -> "ARTH_OP MODULO: %"
  | LPAREN -> "LPAREN: ("
  | RPAREN -> "RPAREN: )"
  | EQUAL -> "EQUAL: ="
  | GREATER_THAN -> "GREATER_THAN: >"
  | LESS_THAN -> "LESS_THAN: <"
  | EQUALS -> "EQUALS: =="
  | GREATER_THAN_EQUAL -> "GREATER_THAN_EQUAL: >="
  | LESS_THAN_EQUAL -> "LESS_THAN_EQUAL: <="
  | NOT_EQUAL -> "NOT_EQUAL: !="
  | AND -> "BOOL_OP AND: &&"
  | OR -> "BOOL_OP OR: ||"
  | NOT -> "BOOL_OP NOT: !"
  | IF -> "KEYWORD: IF"
  | THEN -> "KEYWORD: THEN"
  | ELSE -> "KEYWORD: ELSE"
  | TRUE -> "BOOL_CONST: TRUE"
  | FALSE -> "BOOL_CONST: FALSE"
  | ID id -> "IDENTIFIER: " ^ id
  | INT n -> sprintf "INT_CONST: %d" n
  | STRING str -> sprintf "STRING: \"%s\"" str
  | COMMA -> "COMMA: ,"
  | INDEX -> "TUPLE: INDEX"
  | COUNT -> "TUPLE: COUNT"
  | LEN -> "TUPLE: LEN"
  | ERROR c -> sprintf "ERROR: Unexpected character '%c'" c  
  

let () =
  let lexbuf = Lexing.from_string {|121>=002*#"A_a"Ac(bc,'d){index d}|} in
  let rec ptokens () =
    match Lexer.tokenize lexbuf with
    | EOF -> printf "EOF\n"
    | token ->
      printf "%s\n" (token_string token);
      ptokens ()
  in
  ptokens ()
