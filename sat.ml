open Type;;
open ExtLib;;
open Bdd;;

let rec print_prop a =
    match a with
        | Var a -> print_string a
        | Not a -> print_string "~("; print_prop a; print_string ")"
        | And(a, b) -> print_string "("; print_prop a; print_string " * "; print_prop b; print_string ")"
        | Or(a, b) -> print_string "("; print_prop a; print_string " + "; print_prop b; print_string ")";;

let vars prop =
    let rec vars_aux prop =
        match prop with
            | Var a -> [a]
            | Not a -> vars_aux a
            | And(a, b) -> vars_aux a @ vars_aux b
            | Or(a, b) -> vars_aux a @ vars_aux b
    in
    List.unique (vars_aux prop);;

let rec bdd_from_var_raw ordering varname =
    match ordering with
        | [] -> assert false
        | x :: xs when x = varname -> cnode (terminal, false) (terminal, true)
        | _ :: xs -> let bdd = bdd_from_var_raw xs varname in cnode bdd bdd;;
let bdd_from_var ordering varname = finalize ordering (bdd_from_var_raw ordering varname);;

let not_bdd_raw (bdd, bit) = (bdd, not bit);;
let not_bdd fnode = let (bdd, bit, ord) = unpack fnode in finalize ord (not_bdd_raw (bdd, bit));;

let and_bdd_raw a b = ite_raw a b (terminal, true);;
let and_bdd a b = let (_, _, o) = unpack a in ite a b (bdd_false o);;

let or_bdd_raw a b = ite_raw a (terminal, false) b;;
let or_bdd a b = let (_, _, o) = unpack a in ite a (bdd_true o) b;;

let bdd_from_prop prop =
    let ordering = vars prop in
    let rec bdd_from_prop_aux pprop =
        match pprop with
            | Var a -> bdd_from_var_raw ordering a
            | Not a -> not_bdd_raw (bdd_from_prop_aux a)
            | And(a, b) -> and_bdd_raw (bdd_from_prop_aux a) (bdd_from_prop_aux b)
            | Or(a, b) -> or_bdd_raw (bdd_from_prop_aux a) (bdd_from_prop_aux b)
    in
    finalize ordering (bdd_from_prop_aux prop);;

let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Parser.main Lexer.token lexbuf in
            print_prop result; print_newline();
            let bdd = bdd_from_prop result in
            let (bdd, bit, ord) = unpack bdd in
            print_cOBDD (bdd, bit); print_newline(); flush stdout
        done
    with Lexer.Eof ->
        exit 0;;
