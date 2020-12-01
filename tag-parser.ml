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

let rec dup_exist = function
  | [] -> false
  | hd::tl -> List.exists ((=) hd) tl || dup_exist tl;;


let assert_list_unique lst = match (dup_exist lst) with
  | false -> lst
  | true -> raise X_syntax_error;;

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

let rec is_improper_lst = function
  | Pair(a,b) -> is_improper_lst b
  | Nil -> false
  | _ -> true;;

let rec scheme_list_to_ocaml_list scheme_lst=
  if is_proper_lst scheme_lst then proper_list_to_ocaml_list scheme_lst
  else  improper_list_to_ocaml_list scheme_lst;;

let improper_arglist_to_lambdaOpt arglist final_body= 
  let f = (function | Symbol(e) -> e | _ -> raise X_no_match) in
  let lst = List.map f (improper_list_to_ocaml_list arglist) in
  let lst = assert_list_unique lst in
  let lst_last = List.hd (List.rev lst) in
  let lst_without_last = List.rev (List.tl (List.rev lst)) in
    LambdaOpt (lst_without_last, lst_last, final_body);;

let rec flatten_scheme_expr_list = function
  | (Seq(e)::es) -> e @ (flatten_scheme_expr_list es) 
  | (e::es) -> e :: (flatten_scheme_expr_list es) 
  | [] -> [];; 

let rec body_to_expr body = 
  if (List.length body) > 1 then Seq(flatten_scheme_expr_list body)
  else if (List.length body) = 1 then (List.hd body)
  else raise X_syntax_error;;

let wrap_with_quote sexpr = Pair(Symbol "quote",Pair(sexpr,Nil))
let wrap_with_qq sexpr = Pair(Symbol "quasiquote",Pair(sexpr,Nil))


let rec zip lst1 lst2 =
  match lst1,lst2 with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1, h2)::(zip t1 t2)
  | _, _ -> raise X_syntax_error
;;

let lst_last lst = List.hd (List.rev lst) ;;
let lst_without_last lst= List.rev (List.tl (List.rev lst));;
  
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
| Symbol(x) -> 
    if List.exists (fun y-> x == y) reserved_word_list then raise X_no_match
    else Var(x)

(*variadic*)
(* ToDo: check that string list is actual a set (unique params) *)
(* ToDo: check if Pair(body,Nil) is the only case*)

| Pair(Symbol("lambda"), Pair(arglist,body)) -> 
  (match arglist with 
    | Nil -> LambdaSimple([],(body_to_expr (List.map tag_parse (scheme_list_to_ocaml_list body))))
    | Pair(_,_) ->
        if is_proper_lst arglist then 
          LambdaSimple(
            (assert_list_unique (List.map (function | Symbol(e) -> e | _ -> raise X_no_match) (proper_list_to_ocaml_list arglist))),
            (body_to_expr (List.map tag_parse (scheme_list_to_ocaml_list body)))
          )
        else (improper_arglist_to_lambdaOpt arglist 
              (body_to_expr (List.map tag_parse (scheme_list_to_ocaml_list body))))         
    | Symbol(s) -> LambdaOpt([],s,(body_to_expr (List.map tag_parse (scheme_list_to_ocaml_list body))))
    | _ -> raise X_no_match)

(* Or *)
|  Pair(Symbol ("or"), args) ->
    Or(List.map tag_parse (scheme_list_to_ocaml_list args))


(* Define *)
| Pair(Symbol ("define"), Pair(Symbol(x), Pair(value,Nil))) ->
   Def(tag_parse (Symbol(x)),tag_parse value)


(* set! *)
| Pair(Symbol ("set!"), Pair(Symbol(x), Pair(value,Nil))) ->
   Set(tag_parse (Symbol(x)),tag_parse value)

(* begin *)
| Pair(Symbol ("begin"), Nil) ->
   Const(Void)

| Pair(Symbol ("begin"), Pair(x, Nil)) ->
  tag_parse x


| Pair(Symbol ("begin"),Pair(a,b)) ->
   Seq(flatten_scheme_expr_list (List.map tag_parse (scheme_list_to_ocaml_list (Pair(a,b)))))

(*QQ*)
| Pair(Symbol ("quasiquote"), Pair(sexpr,Nil)) -> (macro_QQ sexpr)   
  
(* and *)
| Pair(Symbol ("and"), sexpr) -> (macro_and sexpr)

(* define mit *)
| Pair(Symbol "define",Pair(var_and_arglist,Pair(sexprs,Nil))) ->
    (match var_and_arglist with 
      | Pair(var, arglist) -> tag_parse (Pair(Symbol "define",Pair(var,Pair(Pair(Symbol("lambda"), Pair(arglist,Pair(sexprs,Nil))),Nil))))
      | _ -> raise X_syntax_error)


| Pair(Symbol "pset!", sexpr) -> macro_pset sexpr

(* Aplic *)
(* ToDo: check input ((((x)))) *)
| Pair(proc, args) ->
    Applic((match (tag_parse proc) with 
            | Const(_) ->  raise X_syntax_error
            | e -> e
        ),List.map tag_parse (scheme_list_to_ocaml_list args))

| _ -> raise X_syntax_error

and macro_QQ = function
    | Pair(Symbol "unquote",Pair(a,Nil)) -> (tag_parse a)
    | Pair(Symbol "unquote-splicing", Pair(a,Nil)) -> raise X_syntax_error 
    | Pair(Pair (Symbol "unquote-splicing",Pair(sexpr,Nil)),b) ->
        Applic(Var "append",([tag_parse sexpr;tag_parse (wrap_with_qq b)]))
    | Pair(Pair (Symbol "unquote", Pair (a, Nil)),b) ->
        Applic(Var "cons",([tag_parse a;tag_parse (wrap_with_qq b)]))
    | Pair(Pair(a,b),c) ->
        Applic(Var "cons",([tag_parse (wrap_with_qq (Pair(a,b)));tag_parse (wrap_with_qq c)]))
    | Pair(a,b) -> 
        Applic(Var "cons",([tag_parse (wrap_with_quote a);tag_parse (wrap_with_qq b)]))
    | s -> tag_parse (wrap_with_quote s)

and macro_and  = function
  | Nil -> tag_parse (Bool true)
  | Pair(a,Nil) -> tag_parse a
  | Pair(a,b) -> If(tag_parse a,macro_and b,tag_parse (Bool false))
  | _ -> raise X_syntax_error

and macro_pset sexpr = 
  let rec make_vars_scheme_list = function
    | Pair(Pair(Symbol(v),_),rest) ->  (tag_parse (Symbol(v))) :: (make_vars_scheme_list rest)
    | Nil -> [Const(Void)] 
    | _ -> raise X_syntax_error in
  let rec make_sexprs_scheme_list = function
    | Pair(Pair(Symbol(v),Pair(sexpr,Nil)),rest) ->  (tag_parse sexpr) :: (make_sexprs_scheme_list rest)
    | Nil -> [Const(Void)] 
    | _ -> raise X_syntax_error in
  (* ToDo: check if need to remove last item  *)
  let vars_scheme_list = lst_without_last (make_vars_scheme_list sexpr) in
  let sexprs_scheme_list = lst_without_last (make_sexprs_scheme_list sexpr) in

  let lambda_arglist = List.map (function | Var(x) -> Var(x ^ String.make 1 '_') | _ -> raise X_syntax_error) vars_scheme_list  in 
  let lambda_inner_body = 
    List.map (function (a,b) -> Set(a,b)) (zip vars_scheme_list lambda_arglist) in

  (* let lambda_inner_body = lambda_inner_body @ [(Const(Sexpr(Bool false)))] in *)  
  (* create the expr: (not send it to tag_parse recursively)*)
  (*reanme expr_i to expr_i_i *)
  let lambda_arglist_str = List.map (function | Var(x) -> (x ^ String.make 1 '_') | _ -> raise X_syntax_error) vars_scheme_list in
  let l2 = LambdaSimple(lambda_arglist_str,(body_to_expr lambda_inner_body)) in
  let l1 = LambdaSimple([],Applic(l2,sexprs_scheme_list)) in
  let app_pset = Applic(l1,[]) in
  app_pset;;
  (* let if_expr = If(app_pset,Const(Sexpr(Bool false)),Const(Void)) in *)
  (* if_expr *)



let tag_parse_expression sexpr = 
   tag_parse sexpr;; 
  (* raise X_not_yet_implemented;; *)

let tag_parse_expressions sexpr = 
  List.map tag_parse_expression sexpr;;
  (* raise X_not_yet_implemented;; *)

  
end;; (* struct Tag_Parser *)
