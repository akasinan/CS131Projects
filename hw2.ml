type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
  
let convert_grammar gram1 =
	let rec alternative_list nonterm rules = match rules with
	| [] -> []
	| (start_symbol, righthandside)::tl -> if nonterm = start_symbol then righthandside::(alternative_list nonterm tl) else alternative_list nonterm tl in
	((fst gram1), fun nonterm -> (alternative_list nonterm (snd gram1)));;

let parse_prefix gram accept frag =
	let rec element_matcher	rules rule accept derivation frag = match rule with
	| [] -> accept derivation frag
	| _ -> match frag with
		| [] -> None
		| prefix::rest_frag ->  match rule with
			| [] -> None
			| (T term)::righthandside -> if prefix = term then (element_matcher rules righthandside accept derivation rest_frag) else None
			| (N nonterm)::righthandside -> (matcher nonterm rules (rules nonterm) (element_matcher rules righthandside accept) derivation frag)
	and matcher start_symbol rules matching_rules accept derivation frag = match matching_rules with
	| [] -> None
	| first_rule::rest_rules -> match (element_matcher rules first_rule accept (derivation@[start_symbol, first_rule]) frag) with
		| None -> matcher start_symbol rules rest_rules accept derivation frag
		| Some result -> Some result in
	matcher (fst gram) (snd gram) ((snd gram) (fst gram)) accept [] frag;;