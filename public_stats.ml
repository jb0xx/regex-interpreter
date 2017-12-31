(* test NFA stats *)

let rec string_of_int_tuple_list l =
  let string_of_int_tuple (a,b) = "(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")" in
    let rec string_of_elements l = match l with
      [] -> ""
      | (h::[]) -> string_of_int_tuple h
      | (h::t) -> string_of_int_tuple h ^ ";" ^ string_of_elements t
    in "[" ^ string_of_elements l ^ "]"
;;

let string_of_stats_tuple (a,b,c) = "(" ^ string_of_int a ^ "," ^ string_of_int b ^ "," ^ string_of_int_tuple_list c ^ ")";;

#use "nfa.ml"

let m = Nfa.make_nfa 0 [1] [(0, Some 'a', 1)];;
print_endline ( string_of_stats_tuple (Nfa.stats m))

let m = Nfa.make_nfa 0 [2] [(0, Some 'a', 1); (0, Some 'b', 2)];;
print_endline ( string_of_stats_tuple (Nfa.stats m))

let m = Nfa.make_nfa 0 [2;3] [(0, Some 'a', 1); (0, Some 'b', 2); (1, Some 'c', 3); (2, Some 'a', 0); (2, Some 'b', 2); (2, Some 'c', 3)];;
print_endline (string_of_stats_tuple (Nfa.stats m))
