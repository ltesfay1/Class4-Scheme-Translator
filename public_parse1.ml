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
	"42" ;
	"#t" ;
	"null" ;
	"(+ 1 2)" ;
	"(+ 1 2 3)" ;
	"(- 3 4)" ;
	"(- 3 4 5)" ;
	"(= 1 2)" ;
	"(= 1 1)" ;
	"(boolean? #t)" ;
	"(boolean? 3)" ;
	"(if #t 1 2)" ;
	"(if #f 1 2)" ;
	"(cons 1 null)" ;
	"(cons 2 (cons 3 null))" ;
	"(car (cons 4 (cons 5 null)))" ;
	"(cdr (cons 6 (cons 7 null)))" ;
] ;;

test_parse s_exprs ;;
