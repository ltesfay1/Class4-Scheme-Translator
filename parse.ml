(* 
   Here is a sample program that uses your scanner & parser
   to convert Scheme code into AST.  It reads one line at a time, 
   converts the line to a list of tokens, feeds the list to your 
   parser, then prints the AST produced in OCaml syntax.

   ocaml parse.ml 
   ocaml parse.ml < SchemeProgramText > SchemeASTs
*)

#use "scheme.ml"

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

let rec toAST () = 
  try
    let line = read_line () in
      let tokens = tokenize line in
      let tree = parse tokens in
      print_endline (unparse2 tree) ; 
      toAST ()
  with End_of_file -> ()
  (*
      | Not_found -> print_endline "Not_found error"
      | Lex_error n -> print_endline "Lex error"
      | Parse_error n -> print_endline "Parse error"
  *)
      | _ -> print_endline "Error"
;;

toAST () ;;
