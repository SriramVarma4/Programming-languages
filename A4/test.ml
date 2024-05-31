open Lexing
open Parser

let parse_input input =
  let lexbuf = Lexing.from_string input in
  try
    parser_main Lexer.tokenize lexbuf
  with
  | Parser.Error ->
      Printf.printf "Syntax error at position %d\n" lexbuf.lex_curr_p.pos_cnum
  | Lexer.Error c ->
      Printf.printf "Lexer error: illegal character '%c'\n" c

let () =
  let input = "predicate(X, Y) :- rule(X), condition(Y)." in
  let program = parse_input input in
  match program with
  | Some clauses ->
      List.iter (fun clause -> print_endline (Ast.string_of_clause clause)) clauses
  | None -> ()
