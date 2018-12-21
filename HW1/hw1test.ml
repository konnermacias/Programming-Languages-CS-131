let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [1] [])
let my_subset_test2 = subset [1;2] [1;2;3]
let my_subset_test3 = not (subset [5;4] [5;1;2])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2] [1;2]
let my_equal_sets_test2 = not (equal_sets [1;3] [3;1;4])

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]
let my_set_union_test2 = equal_sets (set_union [1;2] [3;3;4]) [1;2;3;4]
let my_set_union_test3 = equal_sets (set_union [1;1;2;3] []) [1;2;3]

let my_set_intersection_test0 = 
	equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = 
	equal_sets (set_intersection [1;4;2] []) []
let my_set_intersection_test2 = 
	equal_sets (set_intersection [4;3;2] [3;5;4;4]) [3;4]

let my_set_diff_test0 = equal_sets (set_diff [1] [2;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [4;3] [4;3]) []
let my_set_diff_test2 = equal_sets (set_diff [1;2;3;4;5] [1;2]) [3;4;5]

let my_computed_fixed_point_test0 = 
	computed_fixed_point (=) (fun x -> x mod 3) 15 = 0
let my_computed_fixed_point_test1 = 
	computed_fixed_point (=) (fun x -> x*2) 1000 = 0
let my_computed_fixed_point_test2 = 
	(computed_fixed_point (fun x y -> x > y**2.) (fun x -> x +. 1.) 1.) = 1.


type my_terms = 
	| Phrase | Noun | Verb | Adjective

let gram =
  [Noun, [T"Mary"];
   Noun, [T"Joe"];
   Verb, [T"eats"];
   Verb, [T"drinks"];
   Adjective, [T "red"];
   Phrase, [N Noun; N Verb]]

let my_filter_reachable_test0 =
  filter_reachable (Phrase, gram) =
    (Phrase,
     [Noun, [T "Mary"]; Noun, [T "Joe"]; Verb, [T "eats"];
      Verb, [T "drinks"]; Phrase, [N Noun; N Verb]])
let my_filter_reachable_test1 =
  filter_reachable (Verb, gram) =
    (Verb,
     [Verb, [T "eats"]; Verb, [T "drinks"]])




	 
