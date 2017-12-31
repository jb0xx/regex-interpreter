(* CMSC 330 / Summer 2016 / Project 3 *)
(* Name: Jung S. Lee *)
(* UID: 112973353 *)

#load "str.cma"

(* ------------------------------------------------- *)
(* MODULE SIGNATURE *)
(* ------------------------------------------------- *)

module type NFA =
  sig
    (* You may NOT change this signature *)

    (* ------------------------------------------------- *)
    (* PART 1: NFA IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Abstract type for NFAs *)
    type nfa

    (* Type of an NFA transition.

       (s0, Some c, s1) represents a transition from state s0 to state s1
       on character c

       (s0, None, s1) represents an epsilon transition from s0 to s1
     *)
    type transition = int * char option * int

    (* ------------------------------------------------- *)
    (* Returns a new NFA.  make_nfa s fs ts returns an NFA with start
       state s, final states fs, and transitions ts.
     *)
    val make_nfa : int -> int list -> transition list -> nfa

    (* ------------------------------------------------- *)
    (*  Calculates epsilon closure in an NFA.

	e_closure m ss returns a list of states that m could
	be in, starting from any state in ss and making 0 or
	more epsilon transitions.

       There should be no duplicates in the output list of states.
     *)

    val e_closure : nfa -> int list -> int list

    (* ------------------------------------------------- *)
    (*  Calculates move in an NFA.

	move m ss c returns a list of states that m could
	be in, starting from any state in ss and making 1
	transition on c.

       There should be no duplicates in the output list of states.
     *)

    val move : nfa -> int list -> char -> int list

    (* ------------------------------------------------- *)
    (* Returns true if the NFA accepts the string, and false otherwise *)
    val accept : nfa -> string -> bool

    (* ------------------------------------------------- *)
    (* Gives the stats of the NFA

      the first integer representing the number of states
      the second integer representing the number of final states
      the (int * int) list represents the number of states with a particular number of transitions
      e.g. (0,1) means there is 1 state with 0 transitions, (1,2) means there is 2 states with 1 transition
      the list would look something like: [(0,1);(1,2);(2,3);(3,1)]

    *)

    val stats : nfa -> int * int * (int * int) list

    (* ------------------------------------------------- *)
    (* PART 2: REGULAR EXPRESSION IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    type regexp =
	Empty_String
      | Char of char
      | Union of regexp * regexp
      | Concat of regexp * regexp
      | Star of regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression, print it as a regular expression in
       postfix notation (as in project 2).  Always print the first regexp
       operand first, so output string will always be same for each regexp.
     *)
    val regexp_to_string : regexp -> string

    (* ------------------------------------------------- *)
    (* Given a regular expression, return an nfa that accepts the same
       language as the regexp
     *)
    val regexp_to_nfa : regexp -> nfa

    (* ------------------------------------------------- *)
    (* PART 3: REGULAR EXPRESSION PARSER *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns the
       equivalent regular expression represented as the type regexp.
     *)
    val string_to_regexp : string -> regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns
       the equivalent nfa
     *)
    val string_to_nfa: string -> nfa


    (*
    val get_fs: nfa -> int list
    val get_ts: nfa -> transition list
    val get_states: nfa -> int list
    *)
    (* ------------------------------------------------- *)
    (* Throw IllegalExpression expression when regular
       expression syntax is illegal
     *)
    exception IllegalExpression of string

end

(* ------------------------------------------------- *)
(* MODULE IMPLEMENTATION *)
(* ------------------------------------------------- *)

    (* Make all your code changes past this point *)
    (* You may add/delete/reorder code as you wish
       (but note that it still must match the signature above) *)

module NfaImpl =
struct

type int_tree =
    IntLeaf 
  | IntNode of int * int_tree * int_tree

(* An empty tree is a leaf *)
let empty_int_tree = IntLeaf

type transition = int * char option * int

type nfa = 
    EmptyNFA
  | RealNFA of char list * int_tree * int * int_tree * transition list 
  (* Alphabet, List of States, Start State, List of End States, List of Transitions *)

let rec delete l e = match l with
   [] -> []
   | h::t -> 
      if h = e then delete t e 
      else h::delete t e

let rec distinct l = match l with
   [] -> []
   | h::t -> let t = delete t h in
      h::distinct t

let rec int_mem x t =
  match t with
      IntLeaf -> false
    | IntNode (y,l,r) when x > y -> int_mem x r
    | IntNode (y,l,r) when x = y -> true
    | IntNode (y,l,r) -> int_mem x l

let rec int_insert x t =
  match t with
      IntLeaf -> IntNode(x,IntLeaf,IntLeaf)
    | IntNode (y,l,r) when x > y -> IntNode (y,l,int_insert x r)
    | IntNode (y,l,r) when x = y -> t
    | IntNode (y,l,r) -> IntNode(y,int_insert x l,r)

let rec int_insert_all xs t = match xs with 
    [] -> t
  | u::v -> int_insert_all v (int_insert u t)

let rec int_to_list t = match t with
    IntLeaf -> []
  | IntNode (y,l,r) -> (int_to_list l) @ [y] @ (int_to_list r)


(*Takes a transition list as input and outputs a list of all states *)
let rec get_states ts l = match ts with
    [] -> l
  | (n1,c,n2)::t -> get_states t (n1::n2::l)


let rec get_alphabet ts l = match ts with
    [] -> l
  | (_,None,_)::t -> get_alphabet t l
  | (_,Some c,_)::t -> get_alphabet t (c::l)


let make_nfa ss fs ts = 
  let end_states = int_insert_all fs empty_int_tree in
  let states = int_insert_all (ss::(get_states ts [])) end_states in (*get list of all states of transition and combine with end_states tree*)
  let alphabet = distinct (get_alphabet ts []) in
  RealNFA(alphabet,states,ss,end_states,ts)
  

let e_closure m ss = 
  let rec ec_helper head ans queue = match m with
      EmptyNFA -> empty_int_tree
    | RealNFA (_,_,_,_,ts) -> 
      let rec e_dest_states src ts = match ts with
        [] -> []
      | (s,c,d)::t -> 
        if s = src && c = None && not (int_mem d ans) then 
          d::e_dest_states src t
        else
          e_dest_states src t in

      let next = e_dest_states head ts in
      match distinct queue@next with
          [] -> ans
        | h::t -> ec_helper h (int_insert_all next ans) t in
  match ss with
    [] -> []
  | h::t -> int_to_list (ec_helper h (int_insert_all ss empty_int_tree) t)


let move m ss c = match m with 
    EmptyNFA -> []
  | RealNFA (_,_,_,_,ts) ->  
    let rec move_helper trans state = match trans with
        [] -> []
      | (s,None,d)::t -> move_helper t state
      | (s,Some ch,d)::t -> 
        if s = state && ch = c then 
          d::move_helper t state
        else
          move_helper t state in

    let rec move_each l = match l with
        [] -> []
      | h::t -> (move_helper ts h)@(move_each t) in

    int_to_list (int_insert_all (move_each ss) empty_int_tree)


(*expands string into list of chars*)
let expand s =
  let rec exp_help i l =
    if i < 0 then 
      l 
    else 
      exp_help (i - 1) (s.[i] :: l) in
  exp_help (String.length s - 1) []


(*checks if state n is in NFA m as a final state*)
let final_state m n = match m with
    EmptyNFA -> false
  | RealNFA(_,_,_,fs,_) -> 
    let rec final_help fs = match fs with
        [] -> false
      | h::t -> if h == n then true else final_help t in
    final_help (int_to_list fs)


let accept m s = match m with
    EmptyNFA -> false
  | RealNFA(_,_,ss,fs,ts) -> 
    let c_list = expand s in
    let rec acc_help start l = match l with
        [] -> e_closure m start
      | h::t -> let next = move m (e_closure m start) h in
        acc_help next t in
    let finals = acc_help [ss] c_list in
    let rec fs_check l = match l with
        [] -> false
      | h::t -> (final_state m h) || (fs_check t) in
    fs_check finals


(* gets the sources of a given transition list *)
let rec get_sources ts = match ts with 
    [] -> []
  | (s,c,d)::t -> s::(get_sources t)

(* Returns the number of occurances of n in list l*)
let rec num_occur n l =
  let rec counter l' i = match l' with
      [] -> i
    | h::t -> if n = h then counter t (i + 1) else counter t i in
  counter l 0

(* return length of list l*)
let length l = 
  let rec len l' i = match l' with
      [] -> i
    | h::t -> len t (i + 1) in
  len l 0


let stats n = match n with
    EmptyNFA -> (0,0,[(-1,-1)])
  | RealNFA(_,ls,ss,fs,ts) -> let sources = get_sources ts in
    (* returns an array of num occurances of each element of l1 in l2*)
    let rec occurance_list l1 l2 = match l1 with
        [] -> []
      | h::t -> (num_occur h l2)::(occurance_list t l2) in
    let counts = occurance_list (int_to_list ls) sources in
    let uniq_count = int_to_list (int_insert_all counts empty_int_tree) in
    let count_list = occurance_list uniq_count counts in
    (* maps all (n1,n2) in l1 and l2 *)
    let rec list_mapper l1 l2 = match l1 with
        [] -> []
      | h::t -> match l2 with
          [] -> [(-1,-1)]
        | h'::t' ->  (h,h')::(list_mapper t t') in
    let count_tuple = list_mapper uniq_count count_list in
    let count_states = length (int_to_list ls) in
    let count_fs = length (int_to_list fs) in
    (count_states, count_fs, count_tuple)


type regexp =
	  Empty_String
	| Char of char
	| Union of regexp * regexp
	| Concat of regexp * regexp
	| Star of regexp


let regexp_to_string r = 
  let rec rts_helper reg = match reg with
      Empty_String -> "E"
    | Char(a) -> Char.escaped a
    | Union(a,b) -> String.concat " " [rts_helper a; rts_helper b; "|"]
    | Concat(a,b) -> String.concat " " [rts_helper a; rts_helper b; "."]
    | Star(a) -> String.concat " " [rts_helper a; "*"] in
  rts_helper r


let regexp_to_nfa r =
  (* int -> regexp -> transition_list -> (int * transition_list) *)
  let rec r2n_helper ss reg ts = match reg with
      Empty_String -> (ss+1,(ss,None,ss+1)::ts)
    | Char(a) -> (ss+1,(ss,Some a,ss+1)::ts)
    | Concat(a,b) -> (let concc = r2n_helper ss a ts in
      match concc with
      (n,trans) -> r2n_helper n b trans)
    | Star(a) -> (let starr = r2n_helper ss a ts in
      match starr with
      (n,trans) -> (n+1,(ss,None,n+1)::(n,None,ss)::trans))
    | Union(a,b) -> let unii = r2n_helper ss a ts in
      match unii with
      (m,trans) -> let unii' = (r2n_helper (m+1) b ((ss,None,m+1)::trans)) in
        match unii' with
        (n,trans') -> (n+1, (n,None,n+1)::(m,None,n+1)::trans') in
  
  let ans = r2n_helper 0 r [] in
  match ans with
    (fs,ts) -> make_nfa 0 [fs] ts


(*
let get_fs m = match m with 
    EmptyNFA -> []
  | RealNFA(_,ls,ss,fs,ts) -> int_to_list fs

let get_ts m = match m with
    EmptyNFA -> []
  | RealNFA(_,ls,ss,fs,ts) -> ts

let get_states m = match m with
    EmptyNFA -> []
  | RealNFA(_,ls,ss,fs,ts) -> int_to_list ls
*)



exception IllegalExpression of string

(************************************************************************)
(* PARSER. You shouldn't have to change anything below this point *)
(************************************************************************)

(* Scanner code provided to turn string into a list of tokens *)

type token =
   Tok_Char of char
 | Tok_Epsilon
 | Tok_Union
 | Tok_Star
 | Tok_LParen
 | Tok_RParen
 | Tok_END

let re_var = Str.regexp "[a-z]"
let re_epsilon = Str.regexp "E"
let re_union = Str.regexp "|"
let re_star = Str.regexp "*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"

let tokenize str =
 let rec tok pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_var s pos) then
       let token = Str.matched_string s in
       (Tok_Char token.[0])::(tok (pos+1) s)
	 else if (Str.string_match re_epsilon s pos) then
       Tok_Epsilon::(tok (pos+1) s)
	 else if (Str.string_match re_union s pos) then
       Tok_Union::(tok (pos+1) s)
	 else if (Str.string_match re_star s pos) then
       Tok_Star::(tok (pos+1) s)
     else if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tok (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tok (pos+1) s)
     else
       raise (IllegalExpression "tokenize")
   end
 in
 tok 0 str

(*
  A regular expression parser. It parses strings matching the
  context free grammar below.

   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let lookahead tok_list = match tok_list with
	[] -> raise (IllegalExpression "lookahead")
	| (h::t) -> (h,t)

let rec parse_S l =
	let (a1,l1) = parse_A l in
	let (t,n) = lookahead l1 in
	match t with
		Tok_Union -> (
		let (a2,l2) = (parse_S n) in
		(Union (a1,a2),l2)
		)
		| _ -> (a1,l1)

and parse_A l =
	let (a1,l1) = parse_B l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Char c ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| Tok_Epsilon ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| Tok_LParen ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
	| _ -> (a1,l1)

and parse_B l =
	let (a1,l1) = parse_C l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Star -> (Star a1,n)
	| _ -> (a1,l1)

and parse_C l =
	let (t,n) = lookahead l in
	match t with
   	  Tok_Char c -> (Char c, n)
	| Tok_Epsilon -> (Empty_String, n)
	| Tok_LParen ->
		let (a1,l1) = parse_S n in
		let (t2,n2) = lookahead l1 in
		if (t2 = Tok_RParen) then
			(a1,n2)
		else
			raise (IllegalExpression "parse_C 1")
	| _ -> raise (IllegalExpression "parse_C 2")

let string_to_regexp str =
	let tok_list = tokenize str in
	let (a,t) = (parse_S tok_list) in
	match t with
	[Tok_END] -> a
	| _ -> raise (IllegalExpression "string_to_regexp")

let string_to_nfa s = regexp_to_nfa (string_to_regexp s)

end

module Nfa : NFA = NfaImpl;;
