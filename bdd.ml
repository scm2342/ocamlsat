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

let integersize = 3;; (* integersize in bits *)
let max_i = 1 lsl integersize - 1;; (* maximal storable integer *)
let max_bit = 1 lsl (integersize - 1);; (* msb integer *)
let default_ordering = (List.init integersize (fun x -> 1 lsl (integersize - 1 - x)));;

let union a b =
    let (_ , _, oa) = unpack a in
    let (_ , _, ob) = unpack b in
    assert(oa = ob);
    ite a (bdd_true oa) b;;

(* Currently only works for positive integers *)
let singleton ?ordering:(ordering = default_ordering) integer =
    assert (0 <= integer && integer <= 1 lsl (List.length ordering) - 1);
    let rec singleton_aux partial_ordering partial_cOBDD inbit =
        match partial_ordering with
            | [] -> finalize ordering (partial_cOBDD, inbit)
            | current_bitworth :: next_ordering ->
                 let next_cOBDD, next_inbit =
                     if integer land current_bitworth = 0
                     then
                         cnode (terminal, true) (partial_cOBDD, inbit)
                     else
                         cnode (partial_cOBDD, inbit) (terminal, true)
                 in
                 singleton_aux next_ordering next_cOBDD next_inbit
    in singleton_aux (List.rev ordering) terminal false;;

let union_list ?ordering:(ordering = default_ordering) = List.fold_left (fun x y -> union x (singleton ~ordering:ordering y)) (bdd_false ordering);;

(* iterates over the contents of a set represented by fkt *)
exception WrongGraphSize;;
let iter_contents ?ignore_outofbounds_bits:(ignore_bits = false) f fkt =
    let rec for_all accum lst =
        match lst with
            | [] -> f accum
            | x :: xs -> for_all accum xs; for_all (accum + x) xs
    in
    let rec iter_contents_aux integer_accum partial_ordering partial_cOBDD compl =
        match partial_cOBDD with
            | Terminal -> if compl then () else for_all integer_accum partial_ordering
            | Node(s, u, c, _) ->
                match partial_ordering with
                    | [] -> if ignore_bits then () else raise WrongGraphSize
                    | current_bitworth :: next_partial_ordering -> 
                        let integer = integer_accum + current_bitworth in
                        iter_contents_aux integer_accum next_partial_ordering u (compl <> c);
                        iter_contents_aux integer next_partial_ordering s compl
    in
    let (cOBDD, c, o) = unpack fkt in
    iter_contents_aux 0 o cOBDD c;;

(*BISECT-IGNORE-BEGIN*) (* not tested since auxilliary function and only relies on iter_contents which is tested *)
let print_contents = iter_contents (fun x -> print_endline (string_of_int x));;
(*BISECT-IGNORE-END*)

(* takes a function, a start value and a cOBDD that represents an integerset
 * folds over the elements included in the integerset
 *)
let fold_left_contents ?ignore_outofbounds_bits:(ignore_bits = false) f acc fkt =
    let current_acc = ref acc in
    iter_contents ~ignore_outofbounds_bits:ignore_bits (fun i -> current_acc := f !current_acc i) fkt;
    !current_acc;;

(*BISECT-IGNORE-BEGIN*) (* not tested since only auxilliary function *)
let string_of_contents ?ignore_outofbounds_bits:(ignore_bits = false) = fold_left_contents ~ignore_outofbounds_bits:ignore_bits (fun x y -> x ^ " " ^ string_of_int y) "";;
(*BISECT-IGNORE-END*)

(* takes a cOBDD that represents an integerset.
 * returns the number of elements included in that set
 *)
let cardinal set =
    let rec cardinal_aux layer cOBDD =
        match cOBDD with
            | Terminal, true -> 0
            | Terminal, false -> 1 lsl layer
            | Node(s, u, c, _), icompl ->
                    let new_layer = layer - 1 in
                    let ubit = icompl <> c in
                    let scardinal = cardinal_aux new_layer (s, icompl) in
                    if equal_cOBDD (s, icompl) (u, ubit) then 2 * scardinal else scardinal + cardinal_aux new_layer (u, ubit)
    in
    let (cOBDD, compl, ord) = unpack set in
    cardinal_aux (List.length ord) (cOBDD, compl);;

(* build the complement set *)
let complement_raw (cOBDD, c) = cOBDD, not c;;
let complement set = let (cOBDD, c, ord) = unpack set in finalize ord (cOBDD, not c);;

(* bitwise_ternary_op gets a ternary operator op and two operands that are fnodes
 * op must accept three booleans. the first two are the actual bits of the cOBDD. the last bit is a carry bit. this bit comes from the last recursive call to op
 * op must return a tuple that contains two bits. the first one is the result for the new node (true or false egde) and the second one is the carry that will be handed down
 * it applies op to the edges while handing down the carry
 *
 * this algorithm can do addition and general bitwise operations
 *
 * its result is a tuple of cOBDDs. the first one is the result without any results that come from an addition
 * the second cOBDD is a cOBDD that contains all elements that did overflow with their first bit stripped
 * therefore to simulate a C overflow, one must build the union out of both cOBDDs in the resulting tuple
 *)
exception Overflow;;
let bitwise_ternary_op_raw ?hashconsing:(hashconsing = true) ?raise_overflow:(raise_overflow = false) ?ignore_outofbounds_bits:(ignore_bits = false) maxlayer op operand1 operand2 =
    (* generate terminal cases
     * first element of returned tuple is result without overflow
     * second element is overflow simulation
     *)
    let termcase terminfo width =
        let rec ser ?compl:(compl = false) width =
            match width, compl with
                | 0,_ -> terminal, false
                | _,false -> cnode ~hashconsing:hashconsing (ser ~compl:compl (width - 1)) (terminal, true)
                | _,true -> cnode ~hashconsing:hashconsing (terminal, true) (ser ~compl:compl (width - 1))
        in
        match terminfo with
            | true, false -> (terminal, false), complement_raw (ser width)
            | false, true -> complement_raw (ser ~compl:true width), (terminal, false)
            | true, true -> (terminal, false), (terminal, false)
            (*BISECT-IGNORE-BEGIN*)| false, false -> assert false(*BISECT-IGNORE-END*) (*not tested since just there to avoid compile warning*)
    in

    (* build tuples of subgraphs with the incoming edge type. *)
    let edges (graph, bit) =
        match graph with
            | Node(Terminal, u, c, _) when bit -> [(u, c <> bit), false]
            | Node(s, Terminal, c, _) when c <> bit -> [(s, bit), true]
            | Node(s, u, c, _) -> [(s, bit), true; (u, c <> bit), false]
            | Terminal when bit = false -> [(terminal, false), true; (terminal, false), false]
            | Terminal -> []
    in

    (* build all combinations of edges, apply op and partition them accordingly
     * used explicit recursion in the hopes to be faster
     *)
    let cross_sort carry xs' =
        let rec cross_sort_aux accum_true accum_false xs ys =
            match xs, ys with
                | _, [] -> accum_true, accum_false
                | [], y::ys -> cross_sort_aux accum_true accum_false xs' ys
                | (x,xb)::xs, (y,yb)::ys -> let (new_bit, new_carry) = op xb yb carry in
                    if new_bit then cross_sort_aux ((x, y, new_carry)::accum_true) accum_false xs ((y,yb)::ys) else cross_sort_aux accum_true ((x, y, new_carry)::accum_false) xs ((y,yb)::ys)
        in cross_sort_aux [] [] xs'
    in

    (* do cross_sort over lists
     * used explicit recursion in the hopes that (@) goes through the first list only and conses at the end.
     *)
    let cross_sort_list = (* optimization idea - put a (T, T, _) triple at the front somehow? *)
        let rec cross_sort_list_aux accum_true accum_false triplelist =
            match triplelist with
                | [] -> accum_true, accum_false
                | (graph1, graph2, carry):: triplelist_partial ->
                        let (triples_true, triples_false) = cross_sort carry (edges graph1) (edges graph2) in
                            cross_sort_list_aux (triples_true @ accum_true) (triples_false @ accum_false) triplelist_partial
        in cross_sort_list_aux [] []
    in

    (* takes a list of cOBDD_triples
     * returns a tuple with two booleans
     * the first boolean signals that (T, T, F) is included
     * the second signals that (T, T, T) is included
     *)
    let rec filter_overflow xs =
        match xs with
            | [] -> false, false
            | (y, z, c) :: ys when equal_cOBDD y (terminal, false) && equal_cOBDD z (terminal, false) -> if c then fst (filter_overflow ys), true else true, snd (filter_overflow ys)
            | _ :: ys -> filter_overflow ys
    in

    let rec bitwise_ternary_op_aux layer cOBDD_triples =
        match cOBDD_triples with
            | [] -> (terminal, true), (terminal, true)
            | _ ->  let (resterm, ovterm) = filter_overflow cOBDD_triples in
                    if resterm || ovterm || (ignore_bits && layer = 0) then
                        if raise_overflow && (ovterm || layer <> 0) then raise Overflow else
                        termcase (if ignore_bits then true, true else resterm, ovterm) layer
                    else
                    let new_true_triples, new_false_triples = cross_sort_list cOBDD_triples in
                    let new_layer = layer - 1 in
                    let true_subgraph, true_overflowgraph = bitwise_ternary_op_aux new_layer new_true_triples in
                    let false_subgraph, false_overflowgraph = bitwise_ternary_op_aux new_layer new_false_triples in
                    cnode true_subgraph false_subgraph, cnode true_overflowgraph false_overflowgraph
    in
    bitwise_ternary_op_aux (maxlayer - 1) [operand1, operand2, false];;

let bitwise_ternary_op ?hashconsing:(hashconsing = true) ?raise_overflow:(raise_overflow = false) ?ignore_outofbounds_bits:(ignore_bits = false) op operand1 operand2 =
    let (cOBDD1, compl1, ord1) = unpack operand1 in
    let (cOBDD2, compl2, ord2) = unpack operand2 in
    assert(ord1 = ord2);
    assert(Sort.list (<=) ord1 = ord1);
    let maxlayer = List.length ord1 + 1 in
    let result, overflowgraph = bitwise_ternary_op_raw ~hashconsing:hashconsing ~raise_overflow:raise_overflow ~ignore_outofbounds_bits:ignore_bits maxlayer op (cOBDD1, compl1) (cOBDD2, compl2) in
    finalize ord1 result, finalize ord1 overflowgraph;;

(* a fulladder that can be used as op by bitwise_ternary_op *)
let full_adder a b carry = let a_xor_b = a <> b in a_xor_b <> carry, (a && b) || (carry && a_xor_b);;

(* raw addition that returns a tuple with the result (without overflow) and a fnode containing the integers where an overflow occured *)
let add_raw a b = bitwise_ternary_op full_adder a b;;

(* addition that simulates integer overflows *)
let add_simulate_overflow a b =
    let res, overflow = add_raw a b in
    union res overflow;;

(* addition that raises the Overflow exception when an overflow occures *)
let add_exception_on_overflow a b =
    fst (bitwise_ternary_op ~raise_overflow:true full_adder a b);;

(* this multiplication approximates :( *)
(*BISECT-IGNORE-BEGIN*) (* not tested since not working - see future work multiplicaton *)
let mult op1 op2 = (* maybe here implement only l_shift_once - hand give the shiftet graph to recursive call *)
    let (cOBDD1, compl1, ord1) = unpack op1 in
    let (cOBDD2, compl2, ord2) = unpack op2 in
    assert(ord1 = ord2);
    assert(Sort.list (<=) ord1 = ord1); (* version 1 -> keep ordering restrictons *)
    let maxlayer = List.length ord1 in

    let add layer a b = fst (bitwise_ternary_op_raw ~raise_overflow:true ~ignore_outofbounds_bits:true (layer + 1) full_adder a b) in
    let l_shift_raw op =
        let rec l_shift_raw_aux bits =
            if bits <= 0 then
                op
            else
                cnode (terminal, true) (l_shift_raw_aux (bits - 1))
        in
        l_shift_raw_aux
    in

    let l_shift_once op = cnode (terminal, true) op in

    let string_of_contents op = string_of_contents ~ignore_outofbounds_bits:true (finalize [1;2;4] op) in

    let rec mult_aux layer op op2 =
        let actuallayer = maxlayer - layer in
        let indenter = String.make actuallayer '\t' in
        match op with
            | Terminal, true -> print_endline (indenter ^ "found Terminal, true"); terminal, true
            | Terminal, false when layer = 0 -> print_endline (indenter ^ "found Terminal, false at last layer"); l_shift_raw (terminal, false) maxlayer
            | Terminal, false -> assert false
            | Node(s, u, c, _), compl -> print_endline (indenter ^ "found Node, " ^ string_of_bool compl);
                let ubit = c <> compl in
                let next_layer = layer - 1 in
                let shifted_op = l_shift_once op2 in
                let () = print_endline (indenter ^ "setrecur") in
                let foobar = (mult_aux next_layer (s, compl) shifted_op) in
                let () = print_endline (indenter ^ "add of operand 1: " ^ string_of_contents op2 ^ " operand 2: " ^ string_of_contents foobar) in
                let setrecur = add maxlayer op2 foobar in
                let () = print_endline (indenter ^ "result setrecur: " ^ string_of_contents setrecur) in
                let () = print_endline (indenter ^ "unsetrecur") in
                let unsetrecur = mult_aux next_layer (u, ubit) shifted_op in
                let () = print_endline (indenter ^ "result of unsetrecur: " ^ string_of_contents unsetrecur) in
                let un = ite_raw setrecur (terminal, false) unsetrecur in
                let () = print_endline (indenter ^ "union of setrecur and unsetrecur: " ^ string_of_contents un) in un
    in
    finalize ord1 (mult_aux maxlayer (cOBDD1, compl1) (cOBDD2, compl2));;
(*BISECT-IGNORE-END*)

(* This is a higher order function for use with things like <> &
 * At least for lsb->msb ordering it can easily be extened to allow
 * addition as well by letting it accept a fulladder
 * the result is bitwise_ternary_op
 * this is deprecated and just used for comparison
 *)
(* deprecated - use bitwise_ternary_op *)
let bitwise_binary_op f fkt1 fkt2 =
    let (cOBDD1, c1, o1) = unpack fkt1 in
    let (cOBDD2, c2, o2) = unpack fkt2 in
    assert(o1 = o2);
    let edges (graph, bit) =
        match graph with
            | Node(Terminal, u, c, _) when bit -> [(u, c <> bit), false]
            | Node(s, Terminal, c, _) when c <> bit -> [(s, bit), true]
            | Node(s, u, c, _) -> [(s, bit), true; (u, c <> bit), false]
            | Terminal when bit = false -> [(terminal, false), true; (terminal, false), false]
            | Terminal -> []
    in
    let edgecombi (left, right) =
        let eleft = edges left in
        let eright = edges right in
        let combinations = List.concat (List.map (fun left -> List.map (fun right -> (left, right)) eright) eleft) in
        let setcombi, unsetcombi = List.partition (fun ((_, lbool), (_, rbool)) -> f lbool rbool) combinations in
        let lefts = List.map (fun ((x, _) , (y, _)) -> x, y) in
        lefts setcombi, lefts unsetcombi
    in
    let rec bitwise_binary_op_aux combis =
        let construct_graph combis =
            if combis = []
            then terminal, true
            else
                if List.exists (fun (g1, g2) -> equal_cOBDD g1 (terminal, false) && equal_cOBDD g2 (terminal, false)) combis
                then terminal, false
                else
                    bitwise_binary_op_aux combis
        in
        let new_setcombis, new_unsetcombis =  (fun (x, y) -> (List.concat x, List.concat y )) (List.split (List.map edgecombi combis)) in
        let new_setgraph = construct_graph new_setcombis in
        let new_unsetgraph = construct_graph new_unsetcombis in
        cnode new_setgraph new_unsetgraph
    in
    finalize o1 (bitwise_binary_op_aux [(cOBDD1, c1), (cOBDD2, c2)]);;

(* bitwise not function - can also be done with bitwise_ternary_op
 * test missing XXX *)
(*BISECT-IGNORE-BEGIN*) (* not tested since it can be implemented with bitwise_ternary_op *)
let bitwise_not fkt =
    let rec bitwise_not_aux subgr =
        match subgr with
            | Node(s, u, c, _), i -> cnode (bitwise_not_aux (u, c <> i)) (bitwise_not_aux (s, i))
            | a -> a
    in
    let (cOBDD, inbit, o) = unpack fkt in
    finalize o (bitwise_not_aux (cOBDD, inbit));;
(*BISECT-IGNORE-END*)

(* auxilliary functions *)

(* Converts an integer to a list of booleans *)
let integer2bools ?ordering:(ordering = default_ordering) integer =
    let rec integer2bools_aux partial_ordering =
        match partial_ordering with
            | [] -> []
            | bit :: next_ordering ->
                 (bit land integer != 0) :: integer2bools_aux next_ordering
    in integer2bools_aux ordering;;

let gen_default_ordering ?reverse:(reverse = false) size =
    let ordering = List.init size (fun x -> 1 lsl (size - 1 - x)) in
    if reverse then List.rev ordering else ordering;;

let from_interval_raw size lower upper =
    assert(lower <= upper);
    let ordering = gen_default_ordering size in
    let rec construct partial_ordering smallerside =
        let boundary, smaller_graph, greater_graph = if smallerside then
                lower, (terminal, true), (terminal, false)
            else
                upper, (terminal, false), (terminal, true)
        in
        match partial_ordering with
            | [] -> terminal, false
            | bitpos :: next_partial_ordering ->
                if boundary land bitpos != 0 then
                    cnode (construct next_partial_ordering smallerside) smaller_graph
                else
                    cnode greater_graph (construct next_partial_ordering smallerside)
    in
    let rec same_upper_bits partial_ordering =
        match partial_ordering with
            | [] -> terminal, false
            | bitpos :: next_partial_ordering ->
                let low_bit = lower land bitpos != 0 in
                let up_bit = upper land bitpos != 0 in
                match low_bit, up_bit with
                    | true, true -> cnode (same_upper_bits next_partial_ordering) (terminal, true)
                    | false, false -> cnode (terminal, true) (same_upper_bits next_partial_ordering)
                    | false, true -> cnode (construct next_partial_ordering false) (construct next_partial_ordering true)
                    | true, false -> assert false (* this should not happen - lower bound is greater than upper bound - this is cought by the first assert *)
    in
    same_upper_bits ordering;;

let from_interval size lower upper = (* fix ordering!*)
    let ordering = gen_default_ordering size in
    finalize ordering (from_interval_raw size lower upper);;

(* Pretty printing function that generates a string for use with graphviz dot
 * it does NOT recognize shared subgraphs yet (will implement with hashing) (except for terminal nodes TODO)
 *)
(*BISECT-IGNORE-BEGIN*) (* the following function are auxilliary functions and are therefore not tested *)
let pretty_dot fkt =
    let id_counter = ref 3 in
    let gen_node n partial_ordering = match n with
        | Terminal -> (2, "n2 [shape=box, label=\"T\"];\n")
        | Node(_, _, c, t) ->
            let id = !id_counter in id_counter := id + 1;
            (id, Printf.sprintf "n%d [label=\"%d - %d\"];\n" id (List.hd partial_ordering) t)
    in
    let rec pretty_dot_aux self_id partial_ordering partial_bdd =
        match partial_bdd with
            | Terminal -> [""]
            | Node(s, u, c,_) ->
                let (s_id, s_def) = gen_node s partial_ordering in
                let (u_id, u_def) = gen_node u partial_ordering in
                let dot = Printf.sprintf "%s%sn%d -> n%d\nn%d -> n%d [style=\"%s\"]\n"
                    s_def u_def self_id s_id self_id u_id (if c then "dotted" else "dashed")
                in
                let next_partial_ordering = if partial_ordering = [] then [] else List.tl partial_ordering in
                dot :: pretty_dot_aux s_id next_partial_ordering s
                @ pretty_dot_aux u_id next_partial_ordering u
    in
    let (bdd, c, ordering) = unpack fkt in
    let (id, def) = gen_node bdd ordering in
    let (fstyle, nfstyle) = if c then ("dotted", "solid") else ("solid", "dotted") in
    let dot = Printf.sprintf "n1 [label=\"f\"];\nn0 [label=\"not f\"];\n%sn1 -> n%d [style=\"%s\"];\nn0 -> n%d [style=\"%s\"]\n"
        def id fstyle id nfstyle
    in
    List.fold_left (^) "" ("digraph Bdd {\n" :: dot :: pretty_dot_aux id (List.tl ordering) bdd @ ["}"]);;

(* Generates dot and writes it to a file *)
let pretty_dot_to_file file fkt =
    let fh = open_out file in
    output_string fh (pretty_dot fkt);
    close_out fh;;

(* Generate random graphs *)
let () = Random.self_init ();;
let gen_cOBDD ?nodeprob:(gen_probability_node = 70) ?hashconsing:(hashconsing = true) ?size:(size = integersize) () =
    let rec gen_cOBDD_aux depth_left =
        assert(gen_probability_node <= 100);
        let rand = Random.int 101 in
        if depth_left > 0 && rand <= gen_probability_node then let next_depth = depth_left - 1 in
            let s_node = gen_cOBDD_aux next_depth in
            let u_node = gen_cOBDD_aux next_depth in
            cnode ~hashconsing:hashconsing s_node u_node
        else
            terminal, Random.bool ()
    in
    gen_cOBDD_aux size;;

(* Generate random functions *)
let gen_fkt ?hashconsing:(hashconsing = true) ?ordering:(ordering = default_ordering) () =
    finalize ordering (gen_cOBDD ~hashconsing:hashconsing ~size:(List.length ordering) ());;

(* Generate random functions with default ordering *)
let gen_fkt_size ?hashconsing:(hashconsing = true) ?size:(size = integersize) () =
    let ordering = gen_default_ordering size in
    finalize ordering (gen_cOBDD ~hashconsing:hashconsing ~size:(List.length ordering) ());;

(* polymorphic cross_product.
 * takes a function to apply,
 * an empty representation of the target type,
 * a function that takes an accu of the target type and a result of f and adds f to accu,
 * a next function for the first operand that returns a tuple; the first element of the tuple is an optional type - the second is the extracted element,
 * the first set,
 * the next function for the second operand,
 * the second operand
 *)
let cross_product f empty add_fkt next_y xs next_z =
    let rec cross_product_aux accu ys zs =
        let y, ys' = next_y ys in
        let z, zs' = next_z zs in
        match y, z with
            | _, None -> accu
            | None, _ -> cross_product_aux accu xs zs'
            | Some y', Some z' -> cross_product_aux (add_fkt (f y' z') accu) ys' zs
    in cross_product_aux empty xs;;

(* not used - more of an exercise... *)
module List = struct
    include List;;
    let cross_product f x y = let hdtl zs = match zs with [] -> None, zs | z::zs -> Some z, zs in cross_product f [] (fun x y -> x::y) hdtl x hdtl y;;
end;;

(* not used - more of an exercise... *)
module Set = struct (* is ocaml really this ugly here? *) (* well... if hiding
where to be allowed then one could build something like overriding - not the
ocaml way. this is problematic anyway (rebinding functors :))*)
    module type OrderedType = Set.OrderedType
    module type S = Set.S
    module Make = functor (Elt: OrderedType) -> struct
        include Set.Make(Elt)
        let cross_product f x y =
            let hdtl zs =
                let elem =
                    try
                        Some (choose zs)
                    with Not_found -> None
                in
                elem, match elem with None -> zs | Some ele -> remove ele zs
            in
            cross_product f empty add hdtl x hdtl y;;
    end;;
end;;

(* Functions to manually traverse a cOBDD *)
let go dir bdd =
    match bdd with
    | Node(s, u, c, t) ->
        let _ = Printf.printf "complement was %b and tag %d\n" c t in
        if dir then s else u
    | Terminal -> let _ = Printf.printf "we arrived at the bottom\n\n" in
        terminal;;
let go1 = go true;;
let go0 = go false;;
(*BISECT-IGNORE-END*)

(* these are functions usefull for the new addition
 * union3 should be considerably faster that folding
 * on a bdd list
 *)
let rec union_raw (bdd1, c1) (bdd2, c2) =
    match bdd1, bdd2 with
        | Terminal, _ when not c1 -> terminal, false
        | _, Terminal when not c2 -> terminal, false
        | Terminal, a -> a, c2
        | a, Terminal -> a, c1
        | Node(s1, u1, cu1, _), Node(s2, u2, cu2, _) ->
                cnode (union_raw (s1, c1) (s2, c2)) (union_raw (u1, c1 <> cu1) (u2, c2 <> cu2));;

let rec union_raw3 bdd1 bdd2 bdd3 =
    match bdd1, bdd2, bdd3 with
        | (Terminal, false), _, _ -> terminal, false
        | _, (Terminal, false), _ -> terminal, false
        | _, _, (Terminal, false) -> terminal, false
        | (Terminal, true), a, b -> union_raw a b
        | a, (Terminal, true), b -> union_raw a b
        | a, b, (Terminal, true) -> union_raw a b
        | (Node(s1, u1, cu1, _), c1), (Node(s2, u2, cu2, _), c2),
        (Node(s3, u3, cu3, _), c3) ->
            cnode (union_raw3 (s1, c1) (s2, c2) (s3, c3)) (union_raw3 (u1, c1 <> cu1) (u2, c2 <> cu2) (u3, c3 <> cu3));;

let union3 a b c =
    let (bdda, ca, oa) = unpack a in
    let (bddb, cb, ob) = unpack b in
    let (bddc, cc, oc) = unpack c in
    assert(oa = ob && oa = oc);
    finalize oa (union_raw3 (bdda, ca) (bddb, cb) (bddc, cc));;

let smallest a =
    let (bdda, ca, oa) = unpack a in
    assert(Sort.list (>=) oa = oa);
    let rec smallest_aux ord accu bdd =
        match bdd with
            | Terminal, false -> accu
            | Terminal, true -> assert(false)
            | Node(s, Terminal, uc, _), c when c <> uc -> smallest_aux (List.tl ord) (accu lor (List.hd ord)) (s, c)
            | Node(_, u, uc, _), c -> smallest_aux (List.tl ord) accu (u, c <> uc)
    in
    smallest_aux oa 0 (bdda, ca);;

let greatest a = (* do i WANT a greatest raw i can assume an msg -> lsb ordering here and just go with it; itl still find the greatest element but without knowing what it is... *)
    let (bdda, ca, oa) = unpack a in
    assert(Sort.list (>=) oa = oa);
    let rec greatest_aux ord accu bdd =
        match bdd with
            | Terminal, false -> accu
            | Terminal, true -> assert(false)
            | Node(Terminal, u, uc, _), true -> greatest_aux (List.tl ord) (accu lxor (List.hd ord)) (u, not uc)
            | Node(s, _, _, _), c -> greatest_aux (List.tl ord) accu (s, c)
    in
    greatest_aux oa ((1 lsl List.length oa) - 1) (bdda, ca);;

let rec add_new_raw layer a b =
    match a, b with
        (*| (Terminal, false), (Terminal, false) -> (terminal, false), (terminal,
        true) (*fix overflow?*) add this again? faster?*)
        | (Terminal, true), _ | _, (Terminal, true) -> (terminal, true), (terminal, true)
        | (Terminal, false), a ->
                from_interval_raw layer (smallest (finalize (gen_default_ordering layer) a)) (1 lsl layer - 1), (* think about gen_default_ordering *)
                let gr = (greatest (finalize (gen_default_ordering layer) a) - 1) in if gr < 0 then terminal, true else from_interval_raw layer 0 gr
        | a, (Terminal, false) -> add_new_raw layer (terminal, false) a
        | (Node(s1, u1, cu1, _), c1), (Node(s2, u2, cu2, _), c2) ->
                let ssno, ssov = add_new_raw (layer - 1) (s1, c1) (s2, c2) in
                let suno, suov = add_new_raw (layer - 1) (s1, c1) (u2, c2 <> cu2) in
                let usno, usov = add_new_raw (layer - 1) (u1, c1 <> cu1) (s2, c2) in
                let uuno, uuov = add_new_raw (layer - 1) (u1, c1 <> cu1) (u2, c2 <> cu2) in
                cnode (union_raw3 uuov usno suno) uuno, cnode ssov (union_raw3 usov suov ssno);;

let rec add_new a b =
    let (bdd1, c1, o1) = unpack a in
    let (bdd2, c2, o2) = unpack b in
    assert(o1 = o2);
    assert(Sort.list (>=) o1 = o1);
    let a, b = add_new_raw (List.length o1) (bdd1, c1) (bdd2, c2) in
    finalize o1 a, finalize o2 b

let add_new_simulate_overflow a b =
    let res, overflow = add_new a b in
    union res overflow;;

let rel op a b =
    let (_, _, o1) = unpack a in
    let (_, _, o2) = unpack b in
    assert(o1 = o2);
    let ord_len = List.length o1 in
    let lowest_a = smallest a in
    let greatest_a = greatest a in
    let lowest_b = smallest b in
    let greatest_b = greatest b in
    if op greatest_b lowest_a then
        singleton ~ordering:o1 0, bdd_false o1, bdd_false o2
    else
        if op greatest_a lowest_b then
            singleton ~ordering:o1 1, a, b
        else
            union (singleton ~ordering:o1 0) (singleton ~ordering:o1 1),
            from_interval ord_len lowest_a (if op greatest_a greatest_b then greatest_a else greatest_b), (* do not use from_interval -> or use from_interval + intersection! *)
            from_interval ord_len (if op lowest_b lowest_a then lowest_a else lowest_b) greatest_b

            (*
let filter_max_raw size max bdd =
    match bdd with
        | Terminal, false -> assert(false);
        | Terminal, true -> terminal, false
        | Node(s, u, cu, _), c -> if (1 lsl (size - 1)) land max then cnode (filter max (size - 1) (s, c)) (u,c <> cu) else  (*cont*)
            *)
