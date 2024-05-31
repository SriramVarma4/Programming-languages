type exp =
    Int of int
  | Abs of exp
  | Variable of string
  | Plus of exp * exp
  | Sub of exp * exp
  | Mult of exp * exp
  | Div of exp * exp
  | Mod of exp * exp
  | Exp of exp * exp
  | True
  | False
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
  | Impl of exp * exp
  | Equal of exp * exp
  | Gt of exp * exp
  | Lt of exp * exp
  | Goe of exp * exp
  | Loe of exp * exp
  | Lambda of string * exp
  | Function of exp*exp
  | Ifthenelse of (exp*exp*exp);;

type opcode = 
    INT of int
  | ABS
  | VARIABLE of string
  | PLUS
  | SUB
  | MULT
  | DIV
  | MOD
  | EXP
  | TRUE
  | FALSE
  | NOT
  | AND
  | OR
  | IMPL
  | EQUAL
  | GT
  | LT
  | GOE
  | LOE
  | CLOS of string*opcode list
  | RET
  | APP;;
