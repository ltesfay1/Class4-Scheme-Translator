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
	(* (define next (lambda (y) (+ y 1))) *)
	(List [(Id "define");(Id "next");
		(List [(Id "lambda");
		(List [(Id "y")]);(List [(Id "+");(Id "y");(Num 1)])])]) ;
	(* (next 1) *)
	(List [(Id "next");(Num 1)]) ;
	(* (next 2) *)
	(List [(Id "next");(Num 2)]) ;
	(* (define fact (lambda (n) (if (= n 0) 1 ( * n (fact (- n 1)))))) *)
	(List [(Id "define");(Id "fact");
		(List [(Id "lambda");
		(List [(Id "n")]);(List [(Id "if");
		(List [(Id "=");(Id "n");(Num 0)]);(Num 1);
		(List [(Id "*");(Id "n");
		(List [(Id "fact");
		(List [(Id "-");(Id "n");(Num 1)])])])])])]) ;
	(* (fact 3) *)
	(List [(Id "fact");(Num 3)]) ;
	(* (fact 5) *)
	(List [(Id "fact");(Num 5)]) ;
	(* (define foo (lambda (x) (lambda (y) (+ x y)))) *)
	(List [(Id "define");(Id "foo");
		(List [(Id "lambda");
		(List [(Id "x")]);
		(List [(Id "lambda");
		(List [(Id "y")]);
		(List [(Id "+");(Id "x");(Id "y")])])])]) ;
	(* ((foo 3) 4) *)
	(List [(List [(Id "foo");(Num 3)]);(Num 4)]) ;
] ;; 


test_eval s_exprs ;;
