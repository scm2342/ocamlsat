open ExtLib;;

module Internal: sig
(* cOBDD: complemented ordered decision diagram *)
type cOBDD = private
    (* Terminal node is True if not complemented! *)
    | Terminal
    (* Node of setgraph * unsetgraph * unsetcomplement * hash *)
    | Node of cOBDD * cOBDD * bool * int;;

(* Function entry node. Stores the wheter f is to be
 * complemented and the ordering
 *)
type 'a fNode =
    (* FNode of subgraph * setcomplement * ordering *)
    FNode of cOBDD * bool * 'a list;;

(* constant terminal node *)
val terminal: cOBDD;;
(* constant true function *)
val bdd_true: 'a list -> 'a fNode;;
(* constant false function *)
val bdd_false: 'a list -> 'a fNode;;

(* raw_equal traverses both given graphs until it finds a difference
 * then it yields false else true
 *)
val raw_equal: 'a fNode -> 'a fNode -> bool;;

(* equal makes use of the fact that hashconsing provides maximal
 * sharing. Thus it just checks for physical equality in memory
 *)
val equal: 'a fNode -> 'a fNode -> bool;;
val equal_cOBDD: cOBDD * bool -> cOBDD * bool -> bool;;

(* Construct a node with complemented edges
 * Returns the constructed graph plus a bit that
 * signals whether the incoming edge should be
 * complemented - give that to the next cnode call
 * It ensures that the set edge is never complemented
 *)
val cnode: ?hashconsing:bool -> cOBDD * bool -> cOBDD * bool -> cOBDD * bool;;

(* Finalizes a cOBDD by adding a function entry node
 * for f (and thus for not f). Stores the ordering and whether
 * f is to be complemented or not
 *)
val finalize: 'a list -> cOBDD * bool -> 'a fNode;;

(* Unpacks a finalized graph and returns a tuple of the cOBDD, the
 * unsetcomplementbit and the ordering
 *)
val unpack: 'a fNode -> cOBDD * bool * 'a list;;

(* Evaluates a cOBDD for a given bool list
 * it also gets a complemented bit that signals
 * that not f should be evaluated
 *)
val eval: bool -> 'a fNode -> bool list -> bool;;

(* ite (if then else) gets three functions:
 * the if fkt
 * the then fkt
 * the else fkt
 * With this function all binary boolean operators can be
 * easily implemented.
 * F.e.: and cOBDD1 cOBDD2 = ite cOBDD1 cOBDD2 bdd_false
 * F.e.: or  cOBDD1 cOBDD2 = ite cOBDD1 bdd_true cOBDD2
 *)
val ite_raw: cOBDD * bool -> cOBDD * bool -> cOBDD * bool -> cOBDD * bool;;
val ite: 'a fNode -> 'a fNode -> 'a fNode -> 'a fNode;;

val tag: cOBDD -> int;;

(* used to measure nodes in hashtable *)
val length: unit -> int;;

val print_cOBDD: cOBDD * bool -> unit;;
end
=
struct
type cOBDD =
    | Terminal
    | Node of cOBDD * cOBDD * bool * int;;

type 'a fNode =
    FNode of cOBDD * bool * 'a list;;

let terminal = Terminal;;

let tag b = match b with
    | Node(_, _, _, t) -> t
    | Terminal -> 0

module Hashconsing_Table = Weak.Make
    (struct
        (* type for wich hashconsing should work *)
        type t = cOBDD;;

        (* equality while hashconsing *)
        let equal b1 b2 = match b1, b2 with
            | Node(s1, u1, c1, _), Node(s2, u2, c2, _) ->
                  c1 = c2 && s1 == s2 && u1 == u2 (* works because subgraphs are allready shared *)
            (*| _ -> b1 = b2;;*)
(*BISECT-IGNORE-BEGIN*)
            | _ -> assert false;; (* terminal nodes are not included in the hashtable since they are primitive *)
(*BISECT-IGNORE-END*)

        (* hashfunction used by hashconsing *)
        let hash b = match b with
            | Node(s, u, c, _) ->
                  Hashtbl.hash (tag s, tag u, c)
            (*| Terminal -> 17*)
(*BISECT-IGNORE-BEGIN*)
            | Terminal -> assert false;; (* terminal nodes are not included in the hashtable since they are primitive *)
(*BISECT-IGNORE-END*)
    end);;

(*BISECT-IGNORE-BEGIN*) (* this is not tested since it is only a helper function *)
let rec print_cOBDD (graph, bit) =
    match graph with
        | Terminal ->
            print_string "(Terminal, ";
            print_bool bit;
            print_string ")"
        | Node(s, u, c, t) ->
            print_string "(Node (";
            print_cOBDD (s, bit);
            print_string ", ";
            print_cOBDD (u, c <> bit);
            print_string ", ";
            print_bool c;
            print_string ", ";
            print_int t;
            print_string "), ";
            print_bool bit;
            print_string ")";;
(*BISECT-IGNORE-END*)

let current_tag = ref 1;;
let hashconsing_table = Hashconsing_Table.create 101;;

(* the number of elements in hashconsing table *)
let length () = Hashconsing_Table.count hashconsing_table;;

let cnode ?hashconsing:(hashconsing = true) (set_graph, set_bit) (unset_graph, unset_bit) =
    (* for canonicity a complemented set edge is not allowed
     * newunset = set xor unset
     * newset = false
     * newin = set
     *)
    let ubit = set_bit <> unset_bit in
    let sbit = false in
    let ibit = set_bit in
    let tag = if hashconsing then !current_tag else 0 in
    let create_hashed =
        if hashconsing then (fun tentative_node ->
            let hashconsed_node = Hashconsing_Table.merge hashconsing_table tentative_node in
                if hashconsed_node == tentative_node then
                    current_tag := succ tag;
                    hashconsed_node)
        else
            (fun tentative_node -> tentative_node)
    in
    if ubit = sbit && set_graph = Terminal && unset_graph = Terminal
    then Terminal, ibit
    else create_hashed (Node(set_graph, unset_graph, ubit, tag)), ibit;;

let finalize ordering (graph, inbit) = FNode(graph, inbit, ordering);;
let unpack bdd = match bdd with
    FNode(cOBDD, c, ordering) -> (cOBDD, c, ordering);;

let bdd_true ordering = finalize ordering (terminal, false);;
let bdd_false ordering = finalize ordering (terminal, true);;

let raw_equal f1 f2 =
    let (cOBDD1, c1, o1) = unpack f1 in
    let (cOBDD2, c2, o2) = unpack f2 in
    let rec raw_equal_aux cOBDD1 cOBDD2 =
        match cOBDD1, cOBDD2 with
            | Node(s1, u1, c1, _), Node(s2, u2, c2, _) ->
                   c1 = c2 && raw_equal_aux s1 s2 && raw_equal_aux u1 u2
            | Terminal, Terminal -> true
            | _ -> false
    in
    c1 = c2 && o1 = o2 && raw_equal_aux cOBDD1 cOBDD2

let equal_cOBDD (cOBDD1, c1) (cOBDD2, c2) = c1 = c2 && cOBDD1 == cOBDD2

let equal f1 f2 =
    let (cOBDD1, c1, o1) = unpack f1 in
    let (cOBDD2, c2, o2) = unpack f2 in
    o1 = o2 && equal_cOBDD (cOBDD1, c1) (cOBDD2, c2)

let eval complement fkt fullinput =
    let rec eval_aux bdd complemented input =
        match bdd with
            | Terminal -> not complemented
            | Node(s, u, c, _) -> if List.hd input
                then eval_aux s complemented (List.tl input)
                else eval_aux u (c <> complemented) (List.tl input)
    in
    let (bdd, fcompl, _) = unpack fkt in
    eval_aux bdd (complement <> fcompl) fullinput;;

(* This is still a mockup. it will be augmented with memoizing and more terminal cases *)
let ite_raw (icOBDD, ci) (tcOBDD, ct) (ecOBDD, ce) =
    let get_sgraphs (graph, bit) = match graph with
        | Terminal -> (Terminal, bit), (Terminal, bit)
        | Node(s, u, c, _) -> (s, bit), (u, bit <> c)
    in
    let rec ite_aux i t e =
        match i with
        | Terminal, false -> t
        | Terminal, true -> e
        | _ ->
            let (is, iu) = get_sgraphs i in
            let (ts, tu) = get_sgraphs t in
            let (es, eu) = get_sgraphs e in
            cnode (ite_aux is ts es) (ite_aux iu tu eu)
    in
    ite_aux (icOBDD, ci) (tcOBDD, ct) (ecOBDD, ce);;
let ite if_fkt then_fkt else_fkt =
    let (icOBDD, ci, oi) = unpack if_fkt in
    let (tcOBDD, ct, ot) = unpack then_fkt in
    let (ecOBDD, ce, oe) = unpack else_fkt in
    assert(oi = ot && ot = oe); (* ordering must be the same *)
    finalize oi (ite_raw (icOBDD, ci) (tcOBDD, ct) (ecOBDD, ce));;

end;;

include Internal;;
