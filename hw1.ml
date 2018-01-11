let rec searchlist x l = 
match l with
| [] -> false
| hd :: tl -> if x = hd then true else searchlist x tl;;

let rec listlength l = match l with
| [] -> 0
| hd :: tl -> 1+(listlength tl);;

let rec remXfromList x l =
match l with
|[] -> []
| hd :: tl -> let new_tl = remXfromList x tl in
			  if hd = x then new_tl else hd::new_tl;;

let rec subset a b = 
match a with
| [] -> true
| hd :: tl -> if searchlist hd b = true then subset tl b else false;;

let equal_sets a b =
if (subset a b = true)&&(subset b a = true) then true else false;;

let rec set_union a b = 
match a with
| [] -> b
| hd :: tl -> if searchlist hd b = false then set_union tl ([hd] @ b) else set_union tl b;;

let rec set_intersection a b =
match a with
| [] -> []
| hd :: tl -> if searchlist hd b = false then set_intersection tl b else [hd]@(set_intersection tl b);;

let rec set_diff a b =
match a with
| [] -> []
| hd :: tl -> if searchlist hd b = true then set_diff (remXfromList hd tl) b else [hd]@(set_diff tl b);;

let rec computed_fixed_point eq f x =
if eq (f x) x then x else computed_fixed_point eq f (f x);;

let rec funPtimes f p x =
if p = 0 then x else funPtimes f (p-1) (f x);;

let rec computed_periodic_point eq f p x  =
if eq (funPtimes f p x) x then x else computed_periodic_point eq f p (f x);;

let rec while_away s p x =
if p x = false then [] else [x]@(while_away s p (s x));;

let rec nList f s =
if f = 0 then [] else s::(nList (f-1) s);;

let rec rle_decode lp = 
match lp with
| [] -> []
| hd :: tl -> (nList (fst hd) (snd hd))@(rle_decode tl);;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let filter_blind_alleys g =
	let check_symbol sym terminals =
	match sym with
	| T sym -> true
	| N sym -> if List.exists (fun x -> (fst x) = sym) terminals then true else false in
	let rec rhsIsTerminal rhs terminals =
	match rhs with
	| [] -> true
	| hd::tl -> if (check_symbol hd terminals) then true&&(rhsIsTerminal tl terminals) else false in
	let rec makeTerminals rules terminals = 
	match rules with
	| [] -> terminals
	| hd::tl -> if (rhsIsTerminal (snd hd) terminals)&&(not(subset [hd] terminals)) then makeTerminals tl ([hd]@terminals) else makeTerminals tl terminals in
(fst g, List.filter (fun x -> List.exists (fun y -> y = x) (computed_fixed_point (=) (makeTerminals (snd g)) [])) (snd g));;