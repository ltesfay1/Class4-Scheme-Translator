(* CMSC 330 Project - Scheme Parser & Interpreter *)
 
(* NAME: Lidya Tesfaye *)
 
(* To load this file into the OCaml interpreter, type
 
   #use "scheme.ml"
 
   at the OCaml prompt
*)
 
#load "str.cma"

(* Use this as your abstract syntax tree *)
type ast =
  | Id of string
  | Num of int
  | Bool of bool
  | String of string
  | List of ast list

(* An unparser turns an AST back into a string.  You may find this
   unparser handy in writing this project *)
let rec unparse_list = function
  | [] -> ""
  | (x::[]) -> unparse x
  | (x::xs) -> (unparse x) ^ " " ^ (unparse_list xs)

and unparse = function
  | Id id -> id
  | Num n -> string_of_int n
  | Bool true -> "#t"
  | Bool false -> "#f"
  | String s -> "\"" ^ s ^ "\""
  | List l -> "(" ^ unparse_list l ^ ")"

(************************************************************************)
(* Scanner *)

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_True
 | Tok_False
 | Tok_LParen
 | Tok_RParen
 | Tok_END
 
(* 1 char tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
 
(* 2 char tokens *)
let re_true = Str.regexp "#t"
let re_false = Str.regexp "#f"
 
(* variable char tokens *)
let re_id = Str.regexp "[a-zA-Z=*+/<>!?-][a-zA-Z0-9=*+/<>!?-]*"
let re_num = Str.regexp "[-]*[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
 
exception Lex_error of int
 
let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_true s pos) then
       Tok_True::(tokenize' (pos+2) s)
     else if (Str.string_match re_false s pos) then
       Tok_False::(tokenize' (pos+2) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s


(************************************************************************)
(* Your parser goes here *)



let rec parse_S lst = match lst with
[] -> (Bool false,lst)
| h::t -> match h with
| Tok_Id x1 -> (Id x1,t)
| Tok_Num x2 -> (Num x2,t)
| Tok_String x3 -> (String x3,t)
| Tok_True -> (Bool true,t)
| Tok_False -> (Bool false, t)
| Tok_LParen -> let (c,d) = parse_L t in (List c, d)
| Tok_END -> (Bool false,lst)
 

and parse_L lst = match lst with
[] -> ([],lst)
| h::t -> match h with 
| Tok_RParen -> ([],t)
| Tok_END  -> ([],[])
| _ -> let (x,y) = parse_S lst in let (a,b) = parse_L y in ((x::a),b)

(*lst (* TO DO *)*)
;;

type value =
    Val_Num of int
  | Val_Bool of bool
  | Val_String of string
  | Val_Null
  | Val_Cons of value * value
  | Val_Define of (string * value) list
  | Val_Closure of (string * value) list*string*ast

(* The following function may come in handy *)
let rec string_of_value = function
    Val_Num n -> string_of_int n
  | Val_Bool true -> "#t"
  | Val_Bool false -> "#f"
  | Val_String s -> "\"" ^ s ^ "\""
  | Val_Null -> "null"
  | Val_Cons (v1, v2) -> "(cons " ^ (string_of_value v1) ^ " " ^
      (string_of_value v2) ^ ")"
  | Val_Define v -> "<define>"
  | Val_Closure (x,y,z) -> "<closure>"

let parse lst =
  let (ast,lst2) = (parse_S lst) in ast
;;

(************************************************************************)
(* Write your evaluator here *)
let rec fold f a l = match l with
       [] -> a
         | (h::t) -> fold f (f a h) t
   ;;


let rec eval2 env v = match env with

  [] -> if v = "null" then Val_Null else Val_String v
  | (x,y)::t -> if v = x then y else eval2 t v

exception Eval_error of string
let rec eval env ast = match ast with
| Id v -> eval2 env v 
| Num x -> Val_Num x
| Bool y -> Val_Bool y
| String z -> Val_String z
| List a ->  evaluation (eval3 env a) env


and eval3 env list_ast = match list_ast with
| [] -> []
| (Id "define")::(Id h1)::h2::[] -> (Val_String "define")::(Val_String h1)::(eval env h2)::[]
| (Id "lambda")::(List[Id x])::y::[] -> [Val_String "lambda";Val_Closure (env,x,y)]
| (Id "dynamic")::(List[Id x])::y::[] -> [Val_String "dynamic";Val_Closure ([],x,y)]
| (Id "if")::h1::h2::[] -> (match (eval env h1) with 
                                 Val_Bool x -> if x = true then [Val_String "if";eval env h2] else [Val_String "if";Val_Null]  
                                | _ -> [Val_String "if"])


| (Id "if")::h1::h2::h3::[] -> (match (eval env h1) with 
                                 Val_Bool x -> if x = true then [Val_String "if";eval env h2] else [Val_String "if";eval env h3]  
                                | _ -> [Val_String "if"])

| h::t -> (eval env h)::(eval3 env t)

and evaluation l env = match l with
| [] -> Val_Null
| h::t -> match h with 
| Val_String a ->  
  (if a = "+" then Val_Num (fold (fun a1 h1 -> match h1 with 
    | Val_Num b -> a1 + b
    | _ -> 0) 0 t)
  else if a = "-" then (match t with
    [] -> Val_Num 0 
    | h1::t1 -> (match t1 with
      [] -> (match h1 with 
        | Val_Num c -> Val_Num (0 - c)
        | _ -> Val_Num 0)
      | _ -> (match h1 with 
        | Val_Num c1 -> Val_Num (fold (fun a2 h2 -> (match h2 with
          | Val_Num d -> a2 - d
          | _ -> 0)) c1 t1))))
  else if a = "*" then Val_Num (fold (fun a3 h3 -> match h3 with
    | Val_Num c3 -> a3*c3
    | Val_Null -> 0) 1 t)
  else if a = "=" then (match t with 
  [] -> Val_Bool false
  | h1::h2::t2 -> Val_Bool (h1 = h2)
  | _ -> Val_Bool (false))
else if a = "boolean?" then Val_Bool (match t with
  [] -> false

  | h1::t2 -> match h1 with
    | Val_Bool x -> true
    | _ -> false)
else if a = "number?" then Val_Bool (match t with
  [] -> false
  | h1::t2 -> match h1 with
    | Val_Num x -> true
    | _ -> false)
else if a = "string?" then Val_Bool (match t with
  [] -> false
  | h1::t2 -> match h1 with
    | Val_String x -> true
    | _ -> false)
else if a = "pair?" then Val_Bool (match t with
  [] -> false
  | h1::t2 -> match h1 with
    | Val_Cons (x,y) -> true
    | _ -> false)
else if a = "null?" then Val_Bool (match t with
  [] -> false
  | h1::t2 -> (match h1 with
    | Val_Null -> true
    | _ -> false))
else if a = "if" then (match t with
  [] -> Val_Bool false
  | h1::t2 -> h1)
else if a = "display" then (match t with
  [] -> Val_Null
  | h::t -> (match h with 
    | Val_String x -> print_string x; Val_Null
    | Val_Num y -> print_int y; Val_Null
    | _ -> Val_Null))

else if a = "cons" then (match t with
  [] -> Val_Cons (Val_Null,Val_Null)
  | h1::h2::t -> Val_Cons(h1,h2)
  | _ -> Val_Cons(Val_Null,Val_Null))
else if a = "car" then (match t with
  [] -> Val_Null
  | h1::t1 -> match h1 with 
  | Val_Cons (x1,y1) -> x1
  | _ -> Val_Null)
else if a = "cdr" then (match t with
  [] -> Val_Null
  | h1::t1 -> (match h1 with
   | Val_Cons (x2,y2) -> y2
   | _ -> Val_Null)
  | _ -> Val_Null)
else if a = "define" then (match t with
[] -> Val_Null
| h1::h2::t -> (match h1 with 
  | Val_String x -> Val_Define ((x,h2)::env)
  | _ -> Val_Null)
| _ -> Val_Null)
else if a = "lambda" then match t with
| h1::[] -> h1
| _ -> Val_Null
else if a = "dynamic" then match t with
| h1::[] -> h1
| _ -> Val_Null
else 
Val_Null
)
| Val_Closure (x,y,z) -> match t with
  | h::[] -> (eval (((y,h)::x)@env) z)

