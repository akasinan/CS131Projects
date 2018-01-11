let accept_all derivation str = Some (derivation, str)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

type my_nonterminals =
  | A | B | C | D

let my_test_rules =
   [A, [T"a"; N B];
    A, [T"a"; N C; N D];
    A, [T"a"];
    B, [T"b"];
	B, [T"b"; N C];
    B, [T"b"; N B];
    C, [T"c"];
    C, [N D];
    D, [T"d"]]
	
let my_grammar1 = A, my_test_rules

let my_grammar = convert_grammar my_grammar1

let also_my_grammar =
  (A,
   function
     | A ->
         [[T"a"; N C; N D];
          [T"a"; N B];
		  [T"a"]]
     | B ->
	 [[T"b"; N B];
	  [T"b"; N C];
	  [T"b"]]
     | C ->
	 [[N D];
	  [T"c"]]
     | D ->
	 [[T"d"]])
	    
let test_1 = if (parse_prefix my_grammar accept_all ["a"; "c"; "d"]) = (parse_prefix also_my_grammar accept_all ["a"; "c"; "d"]) then true else false

let test_2 = (parse_prefix my_grammar accept_all ["a"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
												 "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "d"]) = Some
   ([(A, [T "a"; N B]); (B, [T "b"])],
    ["b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
     "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
     "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
     "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
     "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
     "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b"; "b";
     "b"; "b"; "b"; "d"])
