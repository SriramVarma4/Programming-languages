exception TypeError;;
open Defs;;

type env = (string*answer) list and answer = Inta of int| Boola of bool | Closure of env*string*(opcode list) | Vclosure of env*exp;;

let abso e = match e with Int(n) -> Inta(abs n)
        |_->raise TypeError;;


let to_int t = match t with
        Inta n -> n
        |_->raise TypeError;;

let to_bool t = match t with
        Boola n -> n
        |_->raise TypeError;;


let imply e = match e with
        (true,false) -> false
        | _ -> true;;

let rec map lst x =
        match (x,lst) with _,[]-> raise (Failure "Not Found")
        | a,((b,v)::ls)-> 
            begin 
                if (a=b) then v 
                else (map ls x)
            end;;

let rec eval_map rho n = match n with
        Int n -> Inta n
        |Abs n -> abso n
        |Variable n -> map rho n
        |Plus(n1,n2) -> Inta ( to_int (eval_map rho n1) + to_int (eval_map rho n2) )
        |Sub(n1,n2) -> Inta ( to_int (eval_map rho n1) - to_int (eval_map rho n2) )
        |Mult(n1,n2) -> Inta ( to_int (eval_map rho n1) * to_int (eval_map rho n2) )
        |Div(n1,n2) -> Inta ( to_int (eval_map rho n1) / to_int (eval_map rho n2) )
        |Mod(n1,n2) -> Inta ( to_int (eval_map rho n1) mod to_int (eval_map rho n2) )
        |Exp(n1,n2) -> Inta (int_of_float ( ( float_of_int(to_int (eval_map rho n1)) ) ** ( float_of_int(to_int (eval_map rho n2)) ) ))
        |True -> Boola true
        |False -> Boola false
        |Not n -> Boola (not (to_bool(eval_map rho n)))
        |And (n1,n2) -> Boola (((to_bool (eval_map rho n1))) && (to_bool (eval_map rho n1)))
        |Or (n1,n2) -> Boola  (((to_bool (eval_map rho n1))) || (to_bool (eval_map rho n2)))
        |Impl (n1,n2) -> Boola (imply ((to_bool ( eval_map rho n1)),to_bool(eval_map rho n2)))
        |Equal (n1,n2) -> Boola ((to_int(eval_map rho n1)) = to_int(eval_map rho n2))
        |Gt (n1,n2) -> Boola    ((to_int(eval_map rho n1)) > to_int(eval_map rho n2))
        |Lt (n1,n2) -> Boola    ((to_int(eval_map rho n1)) < to_int(eval_map rho n2))
        |Goe (n1,n2) -> Boola   ((to_int(eval_map rho n1)) >= to_int(eval_map rho n2))
        |Loe (n1,n2) -> Boola   ((to_int(eval_map rho n1)) <= to_int(eval_map rho n2))
        | Ifthenelse (cond, then_exp, else_exp) ->
            if to_bool (eval_map rho cond) then eval_map rho then_exp
            else eval_map rho else_exp
        |_->raise TypeError;;


let rec eval_closure c = match c with 
        Vclosure(t,e) -> eval_map t e 
        | Inta(n) -> Inta(n) 
        | Boola(true) -> Boola(true) 
        | Boola(false) -> Boola(false)
        |_->raise TypeError;;

let rec krivine (c , s) = match (c,s) with
        ((table,Int n),s) -> Inta n
        |((table,Abs n),s) -> abso n
        |((table,Variable n),s) -> eval_closure(map table n)
        |((table,Plus(n1,n2)),s) -> Inta ( to_int (eval_closure(krivine((table,n1),s))) + to_int (eval_closure(krivine((table,n2),s)) ))
        |((table,Sub(n1,n2)),s) -> Inta ( to_int (eval_closure(krivine((table,n1),s))) - to_int (eval_closure(krivine((table,n2),s)) ))
        |((table, Mult(n1,n2)),s) -> Inta ( to_int (eval_closure(krivine((table,n1),s))) * to_int (eval_closure(krivine((table,n2),s)) ))
        |((table, Div(n1,n2)),s) -> Inta ( to_int (eval_closure(krivine((table,n1),s))) / to_int (eval_closure(krivine((table,n2),s)) ))
        |((table, Mod(n1,n2)),s) -> Inta ( to_int (eval_closure(krivine((table,n1),s))) mod to_int (eval_closure(krivine((table,n2),s)) ))
        |((table, Exp(n1,n2)),s) -> Inta (int_of_float ( ( float_of_int(to_int (eval_closure(krivine((table,n1),s))) )) ** ( float_of_int(to_int (eval_closure(krivine((table,n2),s))) ) )))
        |((table, True),s) -> Boola true
        |((table, False),s)-> Boola false
        |((table, Not n),s) -> Boola (not (to_bool (eval_closure(krivine((table,n),s)))))
        |((table, And (n1,n2)),s) -> Boola ((to_bool (eval_closure(krivine((table,n1),s)))) && (to_bool (eval_closure(krivine((table,n2),s)))))
        |((table, Or (n1,n2)),s) -> Boola ((to_bool (eval_closure(krivine((table,n1),s)))) || (to_bool (eval_closure(krivine((table,n2),s)))))
        |((table, Impl (n1,n2)),s) -> Boola (imply (to_bool (eval_closure( krivine((table,n1),s))),to_bool (eval_closure(krivine((table,n2),s)))))
        |((table, Equal (n1,n2)),s) -> Boola (to_int (eval_closure(krivine((table,n1),s))) = to_int (eval_closure(krivine((table,n2),s))))
        |((table, Gt (n1,n2)),s) -> Boola (to_int (eval_closure(krivine((table,n1),s))) > to_int (eval_closure(krivine((table,n2),s))))
        |((table, Lt (n1,n2)),s) -> Boola (to_int (eval_closure(krivine((table,n1),s))) < to_int (eval_closure(krivine((table,n2),s))))
        |((table, Goe (n1,n2)),s) -> Boola (to_int (eval_closure(krivine((table,n1),s))) >= to_int (eval_closure(krivine((table,n2),s))))
        |((table, Loe (n1,n2) ),s) -> Boola (to_int (eval_closure(krivine((table,n1),s))) <= to_int (eval_closure(krivine((table,n2),s))))
        | (table, Ifthenelse (cond, then_exp, else_exp)), s ->
                if to_bool (eval_closure (krivine ((table, cond), s))) then
                    krivine ((table, then_exp), s)
                else
                    krivine ((table, else_exp), s)
        |((table, Lambda(str,e)),Vclosure(table',e')::s) -> krivine(([str,Vclosure(table',e')]@table,e),s)
        |((table, Function(e1,e2)),s) -> krivine((table,e1),[Vclosure(table,e2)]@s)
        |_->raise TypeError;;



let print_ans = function
  | Inta n -> print_int n; print_newline ()
  | Boola b -> print_string (if b then "true\n" else "false\n")
  | _ -> raise TypeError


let a=Plus(Int(2),Int(7));;
print_ans(krivine (([],a),[]));;
let t6=Abs(Int(-2));;
print_ans(krivine (([],t6),[]));;
let t1=Int(2);;
let t2=Div(Int(8),t1);;
print_ans(krivine (([],t2),[]));;

let t5 = Ifthenelse(Gt(Int(2), Int(1)), Int(10), Int(20));;
print_ans(krivine (([],t5),[]));;

let t12 =Function (Lambda ("x",Int(3)),Div (Int(1) ,Int(0)));;
print_ans(krivine (([],t12),[]));;


let e1 = Function(Lambda("x",Plus(Variable("x"),Int(5))),Int(2));;
let e = Function(Lambda("x",Div(Variable("x"),Int(1))),Int(3));;
let f = Function (Lambda("y",Int(5)),t2);;
let f1 = Function (Lambda("y",Exp(Variable("y"),Int(5))),t2);;
let f2 = Function (Lambda("y",Exp(Variable("y"),Sub(e1,Int(4)))),Int(3));;

print_ans (krivine(([],e1),[]));;
print_ans (krivine(([],e),[]));;
print_ans (krivine(([],f),[]));;
print_ans (krivine(([],f1),[]));;
print_ans (krivine(([],f2),[]));;
