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
	(* (define foo (lambda (y) (+ x y))) *)
	(List [(Id "define");(Id "foo");
		(List [(Id "lambda");
			(List [(Id "y")]);
			(List [(Id "+");(Id "x");(Id "y")])])]) ;
	(* (define bar (dynamic (y) (+ x y))) *)
	(List [(Id "define");(Id "bar");
		(List [(Id "dynamic");
			(List [(Id "y")]);(List [(Id "+");(Id "x");(Id "y")])])]) ;
	(* (foo 2) *)
	(List [(Id "foo");(Num 2)]) ;
	(* (bar 2) *)
	(List [(Id "bar");(Num 2)]) ;
	(* (define x 4) *)
	(List [(Id "define");(Id "x");(Num 4)]) ;
	(* (foo 2) *)
	(List [(Id "foo");(Num 2)]) ;
	(* (bar 2) *)
	(List [(Id "bar");(Num 2)]) ;
] ;; 


test_eval s_exprs ;;
