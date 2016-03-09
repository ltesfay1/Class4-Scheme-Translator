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
	(* 42 *)
	(Num 42) ;
	(* #t *)
	(Bool true) ;
	(* null *)
	(Id "null") ;
	(* (+ 1 2) *)
	(List [(Id "+");(Num 1);(Num 2)]) ;
	(* (+ 1 2 3) *)
	(List [(Id "+");(Num 1);(Num 2);(Num 3)]) ;
	(* (- 3 4) *)
	(List [(Id "-");(Num 3);(Num 4)]) ;
	(* (- 3 4 5) *)
	(List [(Id "-");(Num 3);(Num 4);(Num 5)]) ;
	(* (= 1 2) *)
	(List [(Id "=");(Num 1);(Num 2)]) ;
	(* (= 1 1) *)
	(List [(Id "=");(Num 1);(Num 1)]) ;
] ;; 


test_eval s_exprs ;;
