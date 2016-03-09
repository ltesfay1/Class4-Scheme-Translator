#use "scheme.ml"
#use "testUtils.ml"

let test_eval x = List.fold_left (fun a h -> 
	let v = (eval a h) in (
		print_endline ("% " ^ (unparse h));
		print_endline (string_of_value v);
		match v with 
			| Val_Define e -> e
			| _ -> a
		)) [] x
;;

let s_exprs = [
	(* (boolean? #t) *)
	(List [(Id "boolean?");(Bool true)]) ;
	(* (boolean? 3) *)
	(List [(Id "boolean?");(Num 3)]) ;
	(* (if #t 1 2) *)
	(List [(Id "if");(Bool true);(Num 1);(Num 2)]) ;
	(* (if #f 1 2) *)
	(List [(Id "if");(Bool false);(Num 1);(Num 2)]) ;
] ;; 


test_eval s_exprs ;;
