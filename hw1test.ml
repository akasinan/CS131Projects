let my_subset_test0 = subset [] [1;2;3]
let my_subset_test1 = subset [3;1;3] [1;2;3]
let my_subset_test2 = not (subset [1;3;7] [4;1;3])
let my_subset_test3 = not (subset [1;3;7;44] [4;1;3])
let my_subset_test4 = not (subset [1;3;7;44] [])

let my_equal_sets_test0 = equal_sets [1;2] [2;1;2]
let my_equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])
let my_equal_sets_test2 = not (equal_sets [1] [3;1;3])
let my_equal_sets_test3 = not (equal_sets [] [3;1;3])
let my_equal_sets_test4 = not (equal_sets [1;3;4] [])

let my_set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;3] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [] []) []
let my_set_union_test3 = equal_sets (set_union [1;2] [2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1;2;3] [3;3;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union (set_union [1;2;3] [3;3;3]) [1;4;5]) [1;2;3;4;5]

let my_set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [1;5;3] [1;2;3]) [1;3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3] [3;1;2]) [3;2;1]
let my_set_intersection_test3 =
  equal_sets (set_intersection [1;5;7] (set_intersection [1;2;3] [1;2])) [1]
  
let my_set_diff_test0 = equal_sets (set_diff [1;2] [1;2;2]) []
let my_set_diff_test1 = equal_sets (set_diff [4;3;1;1;2;3] [1;3]) [4;2]
let my_set_diff_test2 = equal_sets (set_diff [1;2;3] []) [1;2;3]
let my_set_diff_test3 = equal_sets (set_diff [] [1;2;3]) []
let my_set_diff_test4 = equal_sets (set_diff [1;2;3](set_diff [1;2;3] [1;2;3])) [1;2;3]
let my_set_diff_test5 = equal_sets (set_diff [] []) []

let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x*x -3*x + 4) 1 = 2
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x) 1 = 1
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x mod 2) 16 = 0

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x*x -3*x + 4) 1 (1) = computed_fixed_point (=) (fun x -> x*x -3*x + 4) 1
let my_computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x *. x -. 1.) 0 1. = 1.
  
let my_while_away_test0 = equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9]
let my_while_away_test1 = equal_sets (while_away ((+) 2) ((=) 10) 10) [10]
let my_while_away_test2 = equal_sets (while_away ((+) 3) ((=) 10) 0) []

let my_rle_decode_test0 = equal_sets (rle_decode [1,"a";2,"b";4,"c"]) ["a";"b";"b";"c";"c";"c";"c"]
let my_rle_decode_test1 = equal_sets (rle_decode []) []

type test_case_nonterminals =
  | A | B | C | D

let my_test_rules =
   [A, [T"a"; N B];
    A, [N A];
    A, [T"a"; N C; N D];
    A, [T"a"];
    B, [N B];
    B, [T"b"];
    B, [T"b"; N B];
    C, [T"c"];
	C, [N C];
    C, [N D];
    D, [T"d"];
    D, [N D]]

let my_grammar = A, my_test_rules

let my_filter_blind_alleys_test0 =
  filter_blind_alleys my_grammar = my_grammar;;

let my_filter_blind_alleys_test1 =
  filter_blind_alleys (A, List.tl my_test_rules) = (A, List.tl  my_test_rules)

let my_filter_blind_alleys_test2 =
  filter_blind_alleys (A,
      [A, [T"a"; N B];
    A, [N A];
    A, [T"a"; N C; N D];
    A, [T"a"];
    B, [N B];
    B, [T"b"; N B];
    C, [T"c"];
	C, [N C];
    C, [N D];
    D, [T"d"];
    D, [N D]])
  = (A,
      [A, [N A];
    A, [T"a"; N C; N D];
    A, [T"a"];
    C, [T"c"];
	C, [N C];
    C, [N D];
    D, [T"d"];
    D, [N D]])

let my_filter_blind_alleys_test3 =
  filter_blind_alleys (A,
      [A, [T"a"; N B];
    A, [N A];
    A, [T"a"; N C; N D];
    A, [T"a"];
    B, [N B];
    B, [T"b"];
    B, [T"b"; N B];
    C, [T"c"];
	C, [N C];
    C, [N D];
    D, [N D]])
  = (A,
      [A, [T"a"; N B];
    A, [N A];
    A, [T"a"];
    B, [N B];
    B, [T"b"];
    B, [T"b"; N B];
    C, [T"c"];
	C, [N C]])