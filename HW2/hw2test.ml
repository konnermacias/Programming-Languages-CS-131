(* 
	PST: Post,
	Q: Question,
	A: Answer,
	SN: Snippet,
	ST: Statement
*)

type my_terms = 
	| PST | Q | A | SN | ST


let accept_all derivation string =
	Some (derivation, string)

let rec contains_statement = function
	|  [] -> false
	| (ST,_)::_ -> true
	| _::rules -> contains_statement rules

let accept_only_non_statements rules frag = 
	if contains_statement rules then
		None
	else
		Some(rules, frag)


let gram =
	(PST,
	 function
	   | PST ->
		  [[N ST; N SN; N Q; N A];
		   [N ST; N SN; N Q];
		   [N SN; N Q; N A];
		   [N SN; N Q];
		   [N ST; N Q; N A];
		   [N ST; N Q];
		   [N Q; N A];
		   [N Q]]
	   | A ->
	      [[N ST; N SN];
	       [N ST; N Q];
	       [N Q];
	       [N SN; N ST]]
	   | Q ->
	      [[T"Why is this happening?"]; [T"Any help?"];
	       [T"Does this help?"]; [T"Can you add more detail?"]]
	   | SN ->
	      [[T"int a = 2;"];[T"''.join(str)"]; 
	       [T"void myFun (int * a) {"];
	       [T"int a = 2; void myFun (int a) { a += 1 }"]]
	   | ST ->
	      [[T"When I print a, I get 2."];[T"I'm new to python"];
	       [T"You forgot to pass variable a by reference!"];
	       [T"It's working for me."]]
	)

let test_1 =
	(parse_prefix gram accept_all 
		["When I print a, I get 2.";
		"int a = 2; void myFun (int a) { a += 1 }";
		"Any help?";
		"You forgot to pass variable a by reference!";
		"void myFun (int * a) {";
		"Why is this happening?"]) = Some
	 ([(PST, [N ST; N SN; N Q; N A]);
   	   (ST, [T "When I print a, I get 2."]);
       (SN, [T "int a = 2; void myFun (int a) { a += 1 }"]);
       (Q, [T "Any help?"]); (A, [N ST; N SN]);
       (ST, [T "You forgot to pass variable a by reference!"]);
       (SN, [T "void myFun (int * a) {"])],
       ["Why is this happening?"])


let test_2 =
	(parse_prefix gram accept_only_non_statements
		["int a = 2; void myFun (int a) { a += 1 }";
		"Any help?";
		"You forgot to pass variable a by reference!";
		"void myFun (int * a) {";
	    "Why is this happening?"]) = Some
	    ([(PST, [N SN; N Q]);
          (SN, [T "int a = 2; void myFun (int a) { a += 1 }"]);
          (Q, [T "Any help?"])],
         ["You forgot to pass variable a by reference!";
          "void myFun (int * a) {"; "Why is this happening?"])
