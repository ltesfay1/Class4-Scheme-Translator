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
	(* (cons 1 null) *)
	(List [(Id "cons");(Num 1);(Id "null")]) ;
	(* (cons 2 (cons 3 null)) *)
	(List [(Id "cons");(Num 2);
		(List [(Id "cons");(Num 3);(Id "null")])]) ;
	(* (car (cons 4 (cons 5 null))) *)
	(List [(Id "car");
		(List [(Id "cons");(Num 4);
		(List [(Id "cons");(Num 5);(Id "null")])])]) ;
	(* (cdr (cons 6 (cons 7 null))) *)
	(List [(Id "cdr");
		(List [(Id "cons");(Num 6);
		(List [(Id "cons");(Num 7);(Id "null")])])]) ;
] ;; 


test_eval s_exprs ;;
