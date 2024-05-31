type symbol = string
type signature = (symbol * int) list
type tree = V of string | C of { node: symbol ; children: tree list }

(*1*)
let rec check_sig signature =
  let rec check_arity = function
    | (_, arity) when arity < 0 -> false
    | _ -> true
  in
  let rec check_duplicates = function
    | [] -> true
    | hd :: tl -> if List.mem hd tl then false else check_duplicates tl
  in
  let arity_valid = List.for_all check_arity signature in
  let symbols_only = List.map fst signature in
  let no_duplicates = check_duplicates symbols_only in
  arity_valid && no_duplicates

(*2*)
let rec wftree signature tree =
  let rec helper = function
    | V _ -> true
    | C { node; children } ->
        match List.assoc_opt node signature with
        | Some arity -> List.length children = arity && List.for_all helper children
        | None -> false
  in
  helper tree


(*3*)
(*Map, foldr, foldl*)
let rec ht tree =
  match tree with
  | V _ -> 0
  | C { node = _; children } -> 1 + List.fold_left (fun acc t -> max acc (ht t)) 0 children


let rec size tree =
  match tree with
  | V _ -> 1
  | C { node = _; children } -> 1 + List.fold_left (fun acc t -> acc + size t) 0 children

let rec vars tree =
  let rec find_vars acc = function
    | V v -> v :: acc
    | C { node = _; children } -> List.fold_left find_vars acc children
  in
  List.sort_uniq compare (find_vars [] tree)

(*4*)
let rec mirror tree =
  match tree with
  | V v -> V v
  | C { node; children } -> C { node; children = List.rev (List.map mirror children) }

(*5*)
let rec subst s tree =
  match tree with
  | V x -> (try List.assoc x s with Not_found -> tree)
  | C { node; children } -> C { node; children = List.map (fun t -> subst s t) children }

(*6*)
let composition s1 s2 =
  let apply_subst s t =
    List.map (fun (x, t') -> (x, subst s t')) t
  in
  s1 @ (apply_subst s2 s1)

(* 7. Function to find the most general unifier *)
exception NOT_UNIFIABLE

let rec mgu t1 t2 =
  let rec unify s t1 t2 =
    match (t1, t2) with
    | (V x, V y) when x = y -> s
    | (V x, _) | (_, V x) -> (x, t2) :: s
    | (C { node = n1; children = c1 }, C { node = n2; children = c2 }) when n1 = n2 ->
        List.fold_left2 unify s c1 c2
    | _ -> raise NOT_UNIFIABLE
  in
  try unify [] t1 t2 with _ -> raise NOT_UNIFIABLE

(*8*)

let () =
    let sig1 = [("a", 0); ("f", 2); ("g", 1); ("b", 0)] in
    let sig2 = [("b", -1); ("f", 2); ("g", 1)] in
    let sig3 = [("d", 5); ("f", 2); ("g", 3)] in
    let sig4 = [("a", 0); ("f", 2); ("g", 1); ("g", 1)] in
    let sig5 = [] in
    let sig6 = [("a", 3); ("f", 2); ("g", 1); ("b", 2)] in 


    let tree0 = C { node = "f"; children = [V "x"] } in
    let tree1 = C { node = "f"; children = [V "x"; C { node = "g"; children = [V "y"] }] } in
    let tree2 = C { node = "f"; children = [V "x"; C { node = "g"; children = [V "y"; V "z"] }] } in
    let tree3 = C { node = "h"; children = [V "x"; V "y"] } in
    let tree4 = C { node = "f"; children = [V "x"; C { node = "g"; children = [V "y"] }; V "z"] } in

    if check_sig sig1 then print_endline "Signature s1 is valid" else print_endline "Signature s1 is invalid";
    if check_sig sig2 then print_endline "Signature s2 is valid" else print_endline "Signature s2 is invalid";
    if check_sig sig3 then print_endline "Signature s3 is valid" else print_endline "Signature s3 is invalid";
    if check_sig sig4 then print_endline "Signature s4 is valid" else print_endline "Signature s4 is invalid";
    if check_sig sig5 then print_endline "Signature s5 is valid" else print_endline "Signature s5 is invalid";
    if check_sig sig6 then print_endline "Signature s6 is valid" else print_endline "Signature s6 is invalid";

    
    Printf.printf "Tree0 is %s\n" (if wftree sig1 tree0 then "well-formed" else "not well-formed");
    Printf.printf "Tree1 is %s\n" (if wftree sig1 tree1 then "well-formed" else "not well-formed");
    Printf.printf "Tree2 is %s\n" (if wftree sig1 tree2 then "well-formed" else "not well-formed");
    Printf.printf "Tree3 is %s\n" (if wftree sig1 tree3 then "well-formed" else "not well-formed");
    Printf.printf "Tree4 is %s\n" (if wftree sig1 tree4 then "well-formed" else "not well-formed");


    Printf.printf "Height of tree0: %d\n" (ht tree0);
    Printf.printf "Size of tree0: %d\n" (size tree0);
    Printf.printf "Variables in tree0: [%s]\n" (String.concat ", " (vars tree0));
    Printf.printf "Height of tree2: %d\n" (ht tree2);
    Printf.printf "Size of tree2: %d\n" (size tree2);
    Printf.printf "Variables in tree2: [%s]\n" (String.concat ", " (vars tree2));

    let rec print_tree = function
      | V v -> Printf.printf "V \"%s\"" v
      | C { node; children } ->
          Printf.printf "C { node = \"%s\"; children = [" node;
          List.iter (fun t -> print_tree t; Printf.printf "; ") children;
          Printf.printf "] }"
    in

    let tree1_mirror = mirror tree1 in
    Printf.printf "mTree: ";
    print_tree tree1_mirror;
    print_endline "";


    let subst1 = [("x", V "a"); ("y", V "b"); ("z", V "c")] in
    let subst2 = [("z", C { node = "d"; children = [] })] in
    let substtree1 = subst subst1 tree1 in
    Printf.printf "sTree: ";
    print_tree substtree1;
    print_endline "";
        
    let comptree1 = composition subst1 subst2 in
    Printf.printf "compTree: ";
    List.iter (fun (x, t) -> Printf.printf "(\"%s\", " x; print_tree t; Printf.printf "); ") comptree1;
    print_endline "";

    let tree5 = C { node = "f"; children = [V "a"; V "b"; V "c"] } in
    let tree6 = C { node = "f"; children = [V "x"; V "y"; V "z"] } in
    let tree7 = C { node = "f"; children = [V "x"; V "y"] } in
    let tree8 = C { node = "f"; children = [V "a"; V "x"] } in
    try
    let mgu_result = mgu tree7 tree8 in
    Printf.printf "Most general unifier: ";
    List.iter (fun (x, t) -> Printf.printf "(\"%s\", " x; print_tree t; Printf.printf "); ") mgu_result;
    with
    NOT_UNIFIABLE -> Printf.printf "Terms are not unifiable\n";;
    print_endline "";

