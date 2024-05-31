%{
  type term =
    | Atom of string * term list
    | Variable of string
    | Constant of int

  type clause =
    | Fact of term
    | Rule of term * term list

  type program = clause list
%}

%token <string> ID VAR
%token <int> INT
%token LPAREN RPAREN COMMA DOT IF THEN ELSE EOF

%start program
%type <Ast.program> program

%%

program:
  | clause_list EOF         { $1 }

clause_list:
  | clause                  { [$1] }
  | clause clause_list      { $1 :: $2 }

clause:
  | atom DOT                { Fact $1 }
  | atom IF atom_list THEN atom DOT  { Rule ($1, $3) }

atom_list:
  | atom                     { [$1] }
  | atom COMMA atom_list     { $1 :: $3 }

atom:
  | ID LPAREN term_list RPAREN  { Atom ($1, $3) }
  | ID                        { Atom ($1, []) }
  | VAR                       { Variable $1 }
  | INT                       { Constant $1 }

term_list:
  | term                     { [$1] }
  | term COMMA term_list     { $1 :: $3 }

term:
  | atom                     { $1 }
  | VAR                      { Variable $1 }
  | INT                      { Constant $1 }
