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
	(* (define x 1) *)
	(List [(Id "define");(Id "x");(Num 1)]) ;
	(* x *)
	(Id "x") ;
	(* (+ x 2) *)
	(List [(Id "+");(Id "x");(Num 2)]) ;
	(* (define x 4) *)
	(List [(Id "define");(Id "x");(Num 4)]) ;
	(* x *)
	(Id "x") ;
	(* (+ x 2) *)
	(List [(Id "+");(Id "x");(Num 2)]) ;
] ;; 


test_eval s_exprs ;;
