#use "nfa.ml"

let test_accept m str =
    print_endline 
        ("accept(" ^ str ^ ") = " ^ 
        (string_of_bool (Nfa.accept m str) ));;

(*let rec print_list l = 
		[] -> ()
	| h::t -> print_int h ; print_string " " ; print_list t;;*)

let str n = string_of_int n

let print_fs m = 
	let l = Nfa.get_fs m in
	let rec printer l = match l with
			[] -> ""
		| h::t ->  print_int h ; print_string " " ; printer t in
	print_endline(printer l)

let print_states m = 
	let l = Nfa.get_states m in
	let rec printer l = match l with
			[] -> ""
		| h::t ->  print_int h ; print_string " " ; printer t in
	print_endline(printer l)

let print_ts m = 
	let ts = Nfa.get_ts m in
	let rec printer l = match l with
			[] -> () 
		| (s1,None,s2)::t -> print_endline ("(" ^ str s1 ^ ",E," ^ str s2 ^ ") ");printer t
		| (s1,Some c,s2)::t -> print_endline ("(" ^ str s1 ^ "," ^ Char.escaped c ^ "," ^ str s2 ^ ") ");printer t in
	printer ts


let r = Nfa.Char('a');;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Union(Nfa.Char('a'),Nfa.Char('b'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Star(Nfa.Char('a'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Concat(Nfa.Char('a'),Nfa.Char('b'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Star(Nfa.Union(Nfa.Char('a'),Nfa.Char('b')));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Star(Nfa.Concat(Nfa.Char('a'),Nfa.Char('b')));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Union(Nfa.Concat(Nfa.Char('a'),Nfa.Char('b')),Nfa.Char('c'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Concat(Nfa.Char('a'),Nfa.Union(Nfa.Char('b'),Nfa.Char('c')));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Union(Nfa.Star(Nfa.Concat(Nfa.Char('a'),Nfa.Char('b'))),Nfa.Char('c'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Star(Nfa.Star(Nfa.Char('a')));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Star(Nfa.Star(Nfa.Char('a')));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Union(Nfa.Union(Nfa.Char('a'),Nfa.Char('a')),Nfa.Char('b'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;


let r = Nfa.Concat(Nfa.Concat(Nfa.Char('a'),Nfa.Char('b')),Nfa.Char('a'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Concat(Nfa.Char('b'),Nfa.Star(Nfa.Char('a')));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Concat(Nfa.Star(Nfa.Char('a')),Nfa.Char('b'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Union(Nfa.Char('b'),Nfa.Star(Nfa.Char('a')));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;

let r = Nfa.Union(Nfa.Star(Nfa.Char('b')),Nfa.Char('a'));;
let m = Nfa.regexp_to_nfa r;;
print_fs m;;
print_ts m;;
print_states m;;
print_endline("");;



