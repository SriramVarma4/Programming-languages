(* Opcode list*)
type opcode =  INT of int | BOOL of bool | LOOKUP of string | CLOS of string*(opcode list) | CALL | RET | ABSOLUTE 
| NOT| ADD| SUB| DIV| MUL| MOD| EXP| AND| OR| IMP | Equal| GTEqual | LTEqual | Gt | Lt | TUP | PROJ| COND of (opcode list)*(opcode list);;

(* exp definitions *)
type exp = Int of int | Bool of bool | Var of string | Abs of string * exp | App of exp * exp | Absolute of exp| Not of exp
	| Add of exp*exp| Sub of exp*exp| Div of exp*exp| Mul of exp*exp| Mod of exp*exp| Exp of exp*exp
	| And of exp*exp| Or of exp*exp| Imp of exp*exp
	| Equal of exp*exp| GTEqual of exp*exp| LTEqual of exp*exp| Gt of exp*exp| Lt of exp*exp
	| Tup of exp list| Proj of exp*exp | Ifthenelse of (exp*exp*exp);;

(* Interdependent types *)
type table = (string * answer) list 
			and answer = I of int | B of bool | Vclos of table*string*control | T of answer list
			and stack = answer list
			and environment = table
			and control = opcode list
			and dump = ( stack*environment*control )list;;

let _env = [("x", I 3); ("y", I 5); ("z", B true)];;

(* exceptions *)
exception InvalidOperation;;
exception Variable_not_intialized;;
exception ErrorInExecutionSECD;;
exception JoinError;;
exception StackError;;


(* useful functions*)
let rec join (s,n) = match (s,n) with (s,0) -> [] 
					| (n1::s',n) -> (join (s',(n-1)))@[n1] 
					| _-> raise JoinError;;
let rec sl (s',n1) = match (s',n1) with (s',0) -> s' 
							| (n2::s',n1) -> (sl (s',(n1-1))) 
							| _-> raise StackError;; 
let nth l i = match (l,i) with (l,i) -> (Array.of_list(l)).(i);;
let rec power a b = match (a,b) with
	(a,0) -> 1 
	| (a,b) -> a*(power a (b-1));;
let imp a b = match(a,b) with (true,false) -> false| _-> true;;
let rec lookup x env = match env with
	[] -> raise Variable_not_intialized
	| (str,ans)::env' -> if str = x then ans else lookup x env';;
let rec map2 f l = match l with
	[]->[]
	| x::xs -> (f x)@(map2 f xs);;


(* secdmachine execution function *)
let rec secdmac = function 
	| (x::s, _, [], _) -> x
	| (s, e, INT(i)::c, d) -> secdmac (I(i)::s, e, c, d)
	| (s, e, BOOL(i)::c, d) -> secdmac (B(i)::s, e, c, d)
	| (s, e, LOOKUP(x)::c, d) -> secdmac ((lookup x e)::s, e, c, d)
	| (s, e, CLOS(x, c')::c, d) -> secdmac (Vclos(e, x, c')::s, e, c, d)
	| (x::Vclos(e', x', c')::s, e, CALL::c, d) -> secdmac ([], (x', x)::e', c', (s, e, c)::d)
	| (x::s, e, RET::c, (s', e', c')::d) -> secdmac (x::s', e', c', d)
	| (I(i1)::s, e, ABSOLUTE::c, d) -> secdmac (I(if i1>0 then i1 else ((-1)*i1))::s, e, c, d)
	| (I(i2)::I(i1)::s, e, ADD::c, d) -> secdmac (I(i1+i2)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, SUB::c, d) -> secdmac (I(i1-i2)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, MUL::c, d) -> secdmac (I(i1*i2)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, DIV::c, d) -> secdmac (I(i1/i2)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, EXP::c, d) -> secdmac (I(power i1 i2)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, MOD::c, d) -> secdmac (I(i1 mod i2)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, Equal::c, d) -> secdmac (B(if i1==i2 then true else false)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, GTEqual::c, d) -> secdmac (B(if i1>=i2 then true else false)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, LTEqual::c, d) -> secdmac (B(if i1<=i2 then true else false)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, Lt::c, d) -> secdmac (B(if i1<i2 then true else false)::s, e, c, d)
	| (I(i2)::I(i1)::s, e, Gt::c, d) -> secdmac (B(if i1>i2 then true else false)::s, e, c, d)
	| (I(n1)::s', e, TUP::c', d) -> (match (join (s',n1), sl (s',n1)) with (a,b) -> secdmac (T(a)::b, e, c', d))
	| (B(i1)::s, e, NOT::c, d) -> secdmac (B(not i1)::s, e, c, d)
	| (B(i2)::B(i1)::s, e, AND::c, d) -> secdmac (B(i1 && i2)::s, e, c, d)
	| (B(i2)::B(i1)::s, e, OR::c, d) -> secdmac (B(i1 || i2)::s, e, c, d)
	| (B(i2)::B(i1)::s, e, IMP::c, d) -> secdmac (B(imp i1 i2)::s, e, c, d)
	| (B(true)::s, e, COND(c', c'')::c, d) -> secdmac (s, e, c'@c, d)
	| (B(false)::s, e, COND(c', c'')::c, d) -> secdmac (s, e, c''@c, d)

	| (T(n1)::I(n2)::s', e, PROJ::c', d) -> secdmac ((nth n1 n2)::s', e, c', d)
	| _-> raise InvalidOperation;;

(* compile function *)
let rec compile e = match e with
	| Int(i) -> [INT(i)];
	| Bool(i) -> [BOOL(i)]
	| Var(x) -> [LOOKUP(x)]
	| Abs(x, i2) -> [CLOS(x, (compile i2)@[RET])]
	| App(i1, i2) -> (compile i1)@(compile i2)@[CALL]
	| Absolute t -> (compile t)@[ABSOLUTE]
	| Not t -> (compile t)@[NOT]
	| Add(i1,i2) -> (compile i1)@(compile i2)@[ADD]
	| Sub (e1,e2) -> (compile e1)@(compile e2)@[SUB]
	| Mul (e1,e2) -> (compile e1)@(compile e2)@[MUL]
	| Div (e1,e2) -> (compile e1)@(compile e2)@[DIV]
	| Exp (e1,e2) -> (compile e1)@(compile e2)@[EXP]
	| Mod (e1,e2) -> (compile e1)@(compile e2)@[MOD]
	| And (e1,e2) -> (compile e1)@(compile e2)@[AND]
	| Or (e1,e2) -> (compile e1)@(compile e2)@[OR]
	| Imp (e1,e2) -> (compile e1)@(compile e2)@[IMP]
	| Equal (e1,e2) -> (compile e1)@(compile e2)@[Equal]
	| GTEqual (e1,e2) -> (compile e1)@(compile e2)@[GTEqual]
	| LTEqual (e1,e2) -> (compile e1)@(compile e2)@[LTEqual]
	| Gt (e1,e2) -> (compile e1)@(compile e2)@[Gt]
	| Lt (e1,e2) -> (compile e1)@(compile e2)@[Lt]
	| Tup(e1) -> (map2 compile e1)@[INT(List.length e1);TUP]
	| Proj (e1,e2) -> (compile e2)@(compile e1)@[PROJ]
	| Ifthenelse (b,e1,e2) -> compile(b)@[COND(compile e1, compile e2)];;


(* exec funct and print call*)
let exec oplist = secdmac ([], _env, oplist, []);; 
let exec_print oplist =
  let result = exec oplist in
  match result with
  | I i -> print_endline ("Result: " ^ string_of_int i)
  | B b -> print_endline ("Result: " ^ string_of_bool b)
  | _ -> print_endline "Unsupported result type"

(*test cases*)
let t1 = App(Abs("x", Add(Var "x", Int 1)), Int 5);; 
exec_print (compile t1);;
let t2 = App(Abs("x", Add(Var "x", (Sub(Int 10, Var "x")))), Int 2);; 
exec_print (compile t2);;
let t3 = App(Abs("x", Add(Var "x", (Sub(Ifthenelse(Bool true, Int 9, Int 15), Var "x")))), Int 3);; 
exec_print (compile t3);;