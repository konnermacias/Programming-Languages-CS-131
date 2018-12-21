type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

(* 
   I: Nonterminal symbol, rules
   O: [] but rhs's have been adjusted
 *)
let rec giveRHS sym rules =
	match rules with 
	| [] -> []
	| cur_rule::t ->
		if ((fst cur_rule) = sym) then
			(snd cur_rule)::(giveRHS sym t)
		else
			giveRHS sym t
	;;
(* 
    confirmed using:
    (snd (convert_grammar (Expr, awksub_rules))) Expr;;;
*)
let convert_grammar gram1 = 
	match gram1 with
	| (expr, rules) ->
		(expr, function sym -> giveRHS sym rules)
	;;


(* 
   I: Symbol to be added to derivation, rules, 
	  corresponding rhs of given symbol from rules,
	  acceptor, current derivation, and current fragment 

   O: Some derivation leading to original fragment, and 
      corresponding suffix 
*)
let rec matcher startSym rules rhs accept derivation frag = 
	match rhs with
	| [] -> None (* if all possibilites were exhausted *)
	| h_rhs::t_rhs ->
		(* check whether there is a valid derivation and answer *)
		(* add start symbol into derivation list along with correct rhs *)
		match (res_of_matching rules h_rhs accept (List.append derivation [startSym, h_rhs]) frag) with
		| None -> matcher startSym rules t_rhs accept derivation frag (* try for another rhs *)
		| Some answer -> Some answer
	

	and res_of_matching rules rhs accept derivation frag = 
		match rhs with
		| [] -> accept derivation frag (* exhausted all possibilities *)
		| h_rhs::t_rhs ->
			if frag = [] then None else (* frag could be empty! *)
				match h_rhs with 
				| T sym ->
					(* if target symbol matches frag value, continue on with next frag value *)
					if sym = (List.hd frag) then
						(res_of_matching rules t_rhs accept derivation (List.tl frag))
					else
						None
				| N sym ->
					(* here we update the acceptor,
					   generate new rhs for us to begin scanning through *)
					matcher sym rules (rules sym) (res_of_matching rules t_rhs accept) derivation frag
	;;

(* 
   I: grammar, acceptor, and fragment
   O: returns a matcher for the grammar gram and a fragment frag

   Uses match for better readibility - receives no warning messages
 *)
let parse_prefix gram accept frag = 
	match gram with
	| (startSym, rules) ->
		matcher startSym rules (rules startSym) accept [] frag 
	;;
