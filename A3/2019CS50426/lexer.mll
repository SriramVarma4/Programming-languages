{
  type token =
    | EOF
    | PLUS
    | MINUS
    | TIMES
    | DIVIDE
    | MODULO
    | LPAREN
    | RPAREN
    | EQUAL
    | GREATER_THAN
    | LESS_THAN
    | EQUALS
    | GREATER_THAN_EQUAL
    | LESS_THAN_EQUAL
    | NOT_EQUAL
    | AND
    | OR
    | NOT
    | IF
    | THEN
    | ELSE
    | TRUE
    | FALSE
    | ID of string
    | INT of int
    | STRING of string
    | COMMA
    | INDEX
    | COUNT
    | LEN
    | ERROR of char
}

let num = ['0'-'9']
let num1 = ['1'-'9']
let alph = ['a'-'z' 'A'-'Z']
let  loweralph = ['a'-'z']
let identifier = ( loweralph | '_')  (alph | num | '_' | '\'')*


rule tokenize = parse
  | [' ' '\t' '\r' '\n']        { tokenize lexbuf }   
  | '+'                          { PLUS }
  | '-'                          { MINUS }
  | '*'                          { TIMES }
  | '/'                          { DIVIDE }
  | '%'                          { MODULO }
  | '('                          { LPAREN }
  | ')'                          { RPAREN }
  | '='                          { EQUAL }
  | '>'                          { GREATER_THAN }
  | '<'                          { LESS_THAN }
  | "=="                         { EQUALS }
  | ">="                         { GREATER_THAN_EQUAL }
  | "<="                         { LESS_THAN_EQUAL }
  | "!="                         { NOT_EQUAL }
  | "&&"                         { AND }
  | "||"                         { OR }
  | "!"                          { NOT }
  | "if"                         { IF }
  | "then"                       { THEN }
  | "else"                       { ELSE }
  | "true"                       { TRUE }
  | "false"                      { FALSE }
  | ['A'-'Z']                    { ERROR (Lexing.lexeme_char lexbuf 0) }
  | "index"                      { INDEX }
  | "count"                      { COUNT }
  | "len"                        { LEN }
  | identifier as id             { ID id }
  | num+ as num                { INT (int_of_string num) }
  | '"' ([^'"']* as str) '"'     { STRING str }
  | ','                          { COMMA }
  | eof                          { EOF }
  | _                            { ERROR (Lexing.lexeme_char lexbuf 0) }
