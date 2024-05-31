(* Tokeniser for Toy Calculator *)

(* Define token types *)
type token =
  | Identifier of string
  | Keyword of string
  | BooleanOp of string
  | ArithmeticOp of string
  | ComparisonOp of string
  | StringOp of string
  | Integer of int
  | StringLiteral of string
  | Parenthesis of string
  | Comma

(* Define helper functions *)
let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_digit c =
  c >= '0' && c <= '9'

let is_alpha_num_prime_underscore c =
  is_alpha c || is_digit c || c = '\'' || c = '_'

let rec skip_whitespace str idx =
  if idx < String.length str && (str.[idx] = ' ' || str.[idx] = '\t' || str.[idx] = '\n')
  then skip_whitespace str (idx + 1)
  else idx

(* Tokeniser function *)
let rec tokenize str idx =
  let len = String.length str in
  if idx >= len then []
  else
    let idx = skip_whitespace str idx in
    if idx >= len then []
    else
      let c = str.[idx] in
      if is_alpha c || c = '_' then
        let token_str, new_idx = extract_identifier str idx in
        (match token_str with
        | "if" | "then" | "else" | "let" | "in" -> Keyword token_str
        | _ -> Identifier token_str) :: tokenize str new_idx
      else if is_digit c then
        let token_int, new_idx = extract_integer str idx in
        Integer token_int :: tokenize str new_idx
      else
        (match c with
        | '(' | ')' | '[' | ']' | '{' | '}' -> Parenthesis (String.make 1 c) :: tokenize str (idx + 1)
        | ',' -> Comma :: tokenize str (idx + 1)
        | '+' | '-' | '*' | '/' -> ArithmeticOp (String.make 1 c) :: tokenize str (idx + 1)
        | '=' | '<' | '>' -> ComparisonOp (String.make 1 c) :: tokenize str (idx + 1)
        | '"' -> let token_str, new_idx = extract_string_literal str (idx + 1) in
                 StringLiteral token_str :: tokenize str new_idx
        | _ -> tokenize str (idx + 1))

and extract_identifier str idx =
  let len = String.length str in
  let rec loop i =
    if i < len && is_alpha_num_prime_underscore str.[i] then
      loop (i + 1)
    else i in
  let end_idx = loop idx in
  (String.sub str idx (end_idx - idx), end_idx)

and extract_integer str idx =
  let len = String.length str in
  let rec loop i =
    if i < len && is_digit str.[i] then
      loop (i + 1)
    else i in
  let end_idx = loop idx in
  (int_of_string (String.sub str idx (end_idx - idx)), end_idx)

and extract_string_literal str idx =
  let len = String.length str in
  let rec loop i =
    if i < len && str.[i] <> '"' then
      loop (i + 1)
    else i in
  let end_idx = loop idx in
  (String.sub str idx (end_idx - idx), end_idx + 1) (* Skip closing quote *)

(* Test cases *)
let test_tokenizer str =
  let tokens = tokenize str 0 in
  List.iter (fun token -> match token with
                            | Identifier id -> print_endline ("Identifier: " ^ id)
                            | Keyword kw -> print_endline ("Keyword: " ^ kw)
                            | BooleanOp op -> print_endline ("BooleanOp: " ^ op)
                            | ArithmeticOp op -> print_endline ("ArithmeticOp: " ^ op)
                            | ComparisonOp op -> print_endline ("ComparisonOp: " ^ op)
                            | StringOp op -> print_endline ("StringOp: " ^ op)
                            | Integer num -> print_endline ("Integer: " ^ string_of_int num)
                            | StringLiteral str -> print_endline ("StringLiteral: " ^ str)
                            | Parenthesis p -> print_endline ("Parenthesis: " ^ p)
                            | Comma -> print_endline "Comma") tokens

(* Test cases *)
let () =
  let test1 = "let x = 10 + 20 * (if true then 30 else 40)" in
  let test2 = "let y = \"hello world\"" in
  let test3 = "let z = x = 42" in
  let test4 = "let a = (x < 50) && (y > 0)" in
  test_tokenizer test1;
  test_tokenizer test2;
  test_tokenizer test3;
  test_tokenizer test4
