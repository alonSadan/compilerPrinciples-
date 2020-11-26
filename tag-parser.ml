#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)

(* let rec flatten_Pairs = function
  | Pair(Pair(e,es1),es2) -> (flatten_Pairs (Pair(e,es1)))@(flatten_Pairs es2)
  | Pair(a,b) -> a::(flatten_Pairs b)
  | s -> [s];; *)


let rec chained = function
  | Pair(Pair(e,es1),es2)  -> [e]@(chained es1)@(chained es2)
  | Pair(e,es) -> e::(chained es)
  | Nil -> []
  | e -> [e];;


let chained_Symbol s=
  List.map 
    (function 
      | Symbol(e) -> e
      | _ -> raise X_no_match)
    (chained s);;


let rec improper_list_to_ocaml_list = function
  | Pair(a,b) -> a::(improper_list_to_ocaml_list b)
  | s -> [s];;


let rec proper_list_to_ocaml_list = function
  | Pair(a,b) -> a::(proper_list_to_ocaml_list b)
  | Nil -> []  
  | _ -> raise X_no_match;;  
  
let rec is_proper_lst = function
  | Pair(a,b) -> is_proper_lst b
  | Nil -> true
  | _ -> false;;

let rec is_inproper_lst = function
  | Pair(a,b) -> is_inproper_lst b
  | Nil -> false
  | _ -> true;;

let rec scheme_list_to_ocaml_list scheme_lst=
  if is_proper_lst scheme_lst then proper_list_to_ocaml_list scheme_lst
  else  improper_list_to_ocaml_list scheme_lst;;

let improper_arglist_to_lambdaOpt arglist final_body= 
  let f = (function | Symbol(e) -> e | _ -> raise X_no_match) in
  let lst = List.map f (improper_list_to_ocaml_list arglist) in
  let lst_last = List.hd (List.rev lst) in
  let lst_without_last = List.rev (List.tl (List.rev lst)) in
    LambdaOpt (lst_without_last, lst_last, final_body);;

let rec tag_parse = function 
| Bool(x) -> Const(Sexpr(Bool(x)))
| Char(x) -> Const(Sexpr(Char(x)))
| Number(x) -> Const(Sexpr(Number(x))) 
| String(x) -> Const(Sexpr(String(x)))
| Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil)))) ->
    If(tag_parse test, tag_parse dit, tag_parse dif)
| Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) ->
  If(tag_parse test, tag_parse dit, Const(Void))

| Pair(Symbol("quote"), Pair(x, Nil)) -> Const(Sexpr(x))
| Pair(Symbol("unquote"), Pair(x, Nil)) -> Const(Sexpr(x))
| Pair(Symbol("quasiquote"), Pair(x, Nil)) -> Const(Sexpr(x))
| Pair(Symbol("unquote-splicing"), Pair(x, Nil)) -> Const(Sexpr(x))

| Symbol(x) -> 
    if List.exists (fun y-> x == y) reserved_word_list then raise X_no_match
    else Var(x)

(*variadic*)
(* ToDo: check that string list is actual a set (unique params) *)
(* ToDo: check if Pair(body,Nil) is the only case*)

| Pair(Symbol("lambda"), Pair(arglist,Pair(body,Nil))) -> 
  (match arglist with 
    | Pair(_,_) ->
        if is_proper_lst arglist then 
          LambdaSimple(
            (List.map (function | Symbol(e) -> e | _ -> raise X_no_match) (proper_list_to_ocaml_list arglist)),
            (tag_parse body)
          )
        else (improper_arglist_to_lambdaOpt arglist (tag_parse body))         
    | Symbol(s) -> LambdaOpt([],s,(tag_parse body))
    | _ -> raise X_no_match)

(* Or *)
|  Pair(Symbol ("or"), args) ->
    Or(List.map tag_parse (scheme_list_to_ocaml_list args))


(* Define *)
| Pair(Symbol ("define"), Pair(Symbol(x), Pair(value,Nil))) ->
   Def(Const(Sexpr(Symbol(x))),tag_parse value)


(* set! *)
| Pair(Symbol ("set!"), Pair(Symbol(x), Pair(value,Nil))) ->
   Def(Const(Sexpr(Symbol(x))),tag_parse value)

(* begin *)
| Pair(Symbol ("begin"), Nil) ->
   Const(Void)

| Pair(Symbol ("begin"), Pair(x, Nil)) ->
  tag_parse x

(* | Pair(a,b) -> if  is_inproper_lst Pair(a,b) then Seq(List.map tag_parse (scheme_list_to_ocaml_list x))

| Pair(Symbol("begin"),sexp) -> Seq[tag_parse sexp)

| is_proper_lst x -> List.map tag_parse (scheme_list_to_ocaml_list x) *)

(* | Pair(Symbol ("begin"),x) -> tag_parse x)
   
   (match (tag_parse a),(tag_parse b) with 
    | expr1,Seq(expr2) -> Seq(expr1::expr2) (*explicit*)
    | expr1,expr2 -> Seq([expr1]@[expr2])
    )  *)
   

(*(print-template '(begin 2 (begin 3 4)))*)
(* Pair(Symbol "begin", 
    Pair(Number (Fraction(2, 1)), 
      Pair(
        Pair(Symbol "begin", 
          Pair(Number (Fraction(3, 1)), 
            Pair(Number (Fraction(4, 1)), Nil))), 
      Nil)))      *)

(* Aplic *)
|  Pair(Symbol (x), args) ->
    Applic(Const(Sexpr(Symbol(x))),List.map tag_parse (scheme_list_to_ocaml_list args))


| _ -> raise X_no_match;;




let tag_parse_expression sexpr = 
   tag_parse sexpr;; 
  (* raise X_not_yet_implemented;; *)

let tag_parse_expressions sexpr = 
  List.map tag_parse_expression sexpr;;
  (* raise X_not_yet_implemented;; *)

  
end;; (* struct Tag_Parser *)
