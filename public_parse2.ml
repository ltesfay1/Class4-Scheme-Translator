#use "scheme.ml"
#use "testUtils.ml"

let rec unparse_list2 = function
    [] -> ""
  | (x::[]) -> unparse2 x
  | (x::xs) -> (unparse2 x) ^ ";" ^ (unparse_list2 xs)

and unparse2 = function
  | Id id -> "(Id \"" ^ id ^ "\")"
  | Num n -> "(Num " ^ string_of_int n ^ ")"
  | Bool true -> "(Bool true)"
  | Bool false -> "(Bool false)"
  | String s -> "(String \"" ^ s ^ "\")"
  | List l -> "(List [" ^ unparse_list2 l ^ "])"
;;

let test_parse x = List.map (fun y -> 
	print_endline ("% " ^ y) ;
	print_endline (unparse2 (parse (tokenize y))) ) 
	x
;;

let s_exprs = [
	"(define x 1)" ;
	"x" ;
	"(+ x 2)" ;
	"(define x 4)" ;
	"x" ;
	"(+ x 2)" ;
	"(define next (lambda (y) (+ y 1)))" ;
	"(next 1)" ;
	"(next 2)" ;
	"(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))" ;
	"(fact 3)" ;
	"(fact 5)" ;
	"(define foo (lambda (x) (lambda (y) (+ x y))))" ;
	"((foo 3) 4)" ;
	"(define x 1)" ;
	"(define foo (lambda (y) (+ x y)))" ;
	"(define bar (dynamic (y) (+ x y)))" ;
	"(foo 2)" ;
	"(bar 2)" ;
	"(define x 4)" ;
	"(foo 2)" ;
	"(bar 2)"
] ;;

test_parse s_exprs ;;
