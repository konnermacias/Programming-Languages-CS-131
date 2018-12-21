open Pervasives;;

let subset a b = 
	List.for_all(fun x -> List.mem x b) a;;

let equal_sets a b =
	(subset a b) && (subset b a);;

let rec set_union a b = 
	match a with 
	| [] -> b
	| h::t ->
		if List.mem h b then
			set_union t b
		else
			set_union t (h::b)
	;;

let set_intersection a b =
	List.filter(fun x -> List.mem x b) a;;

let set_diff a b = 
	List.filter(fun x -> not (List.mem x b)) a;;

let rec computed_fixed_point eq f x =
	if eq (f x) x then
	  x
	else
	  computed_fixed_point eq f (f x);;


type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
;;


(*
  General logic:
	1. Begin with start expr, add all rules assoc w/expr
	2. Go thru rules & if they are nonterminal, 
		recursively add all rules associated with that
		term
	3. Sop when remaining set of rules no longer contain
		nonterms
	4. Filter each term if it's in targetList 
*)


(*
val checkNonterminal : ('a, 'b) symbol -> bool = <fun>
*)
let checkNonterminal symbol =
	match symbol with
	| T symbol -> false
	| N symbol -> true
	;;


(* 
val gatherRHS :
  ('a, 'b) symbol ->
  ('a * 'c) list -> ('a * 'c) list -> ('a * 'c) list = <fun>
*)
(* 
	Given a N symbol, will return a symbol list of 
	all corresponding rules
*)
let rec gatherRHS sym rList targetList queue gram =
	match sym with 
	| N sym ->
		match gram with
		| [] -> rList
		| cur_rule::t ->
		(* check that is it not a subset of the queue *)
			if ((fst cur_rule) = sym) && (not (subset [cur_rule] targetList)) && (not (subset [cur_rule] queue)) then
				gatherRHS (N sym) (cur_rule::rList) targetList queue t
			else
				gatherRHS (N sym) rList targetList queue t
	;;


(* 
val helper :
  ('a, 'b) symbol list list ->
  ('a * 'c) list -> 'd -> ('a * 'c) list -> ('a * 'c) list = <fun>
*)

(*
	Given a symbol list, will traverse through

	if symbol is N, it will call gatherRHS to 
	return a symbol list of all rules not already 
	in mega set.

	If not, we keep the queue the same, but now
	iterate to next elm.

	Adds all rules to queue of nonterminals

	I: rhs of a rule
	O: queue
*)


(* rhs: [N Lvalue; N Binop; N Expr] queue; [N Expr]) *)
let rec helper rhs queue targetList gram = 
	match rhs with 
	| [] -> queue
	| h::t ->
		(* h -> N Lvalue *)
		(* CODE SHOULD BE USED WHEN HAVING ONE RHS *)
		if  (checkNonterminal h) then
			helper t (List.append queue (gatherRHS h [] targetList queue gram)) targetList gram
		else
			(* go onto next rule in curGroup but add term rule to mega *)
			helper t queue targetList gram 
	;;

(*
val check :
  ('a * ('a, 'b) symbol list list) list ->
  ('a * ('a, 'b) symbol list list) list ->
  ('a * ('a, 'b) symbol list list) list ->
  ('a * ('a, 'b) symbol list list) list = <fun>
 *)

 (* 
 	Given a queue of rules, iterate through and check if rule
 	is has an empty rhs. If not,

 	I: queue of rules
 	O: result set of all reachable rules
 *)
let rec check queue targetList gram =
	(* curGroup [(Expr, [N Lvalue; N Incrop]); (Expr, [N Incrop...])] *) 
	match queue with
	| [] -> targetList
	| cur_rule::t ->
		(* need to check for (Expr, [N Expr; N Binop; N Expr]) *)
		if ((snd cur_rule) <> []) then
			check (helper (snd cur_rule) t targetList gram) (cur_rule::targetList) gram
		else
			check t (cur_rule::targetList) gram
	;;


(* returns list of rhs given nonterminal symbol *)
(*
val giveRHS : 'a -> ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list =
  <fun>
*)
(* targetlist isn't important here *)
let rec giveRHS nonTerm targetList grammar = 
	match grammar with
	| [] -> targetList
	(* snd gg given ex. (Snore, [T "ZZZ"]) *)
	| cur_rule::t ->
		if ((fst cur_rule) = nonTerm) && (not (subset [cur_rule] targetList)) then
			giveRHS nonTerm (cur_rule::targetList) t
		else
			giveRHS nonTerm targetList t
	;;

(* 
val filter_reachable :
  'a * ('a * ('a, 'b) symbol list) list ->
  'a * ('a * ('a, 'b) symbol list) list = <fun>
*)

(* 1. giveRHS (fst g) [] (snd g)) generates all rules with first expr
   2. check [rules] [] (snd g) will fill out [] with all reachable rules
   3. Then we check if the rule is within the set of reachable rules *)
let filter_reachable g = 
	(fst g, List.filter (fun x -> List.mem x ( check (giveRHS (fst g) [] (snd g)) [] (snd g) )) (snd g))
	;;