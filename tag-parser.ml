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
|Pair(Symbol("lambda"), Pair(Pair(args,Pair(Symbol(arg),Symbol(argOptional))),Pair(body,Nil))) ->
  LambdaOpt((chained_Symbol args)@[arg], argOptional,tag_parse body)

|Pair(Symbol("lambda"), Pair(Symbol (x), body)) -> LambdaOpt([],x,tag_parse body)

|Pair(Symbol("lambda"), Pair(Pair(args,Nil), Pair(body,Nil))) -> 
  LambdaSimple(chained_Symbol args,tag_parse body)

| _ -> raise X_no_match;;


(* | LambdaSimple of string list * expr
| LambdaOpt of string list * string * expr *)

(* Pair(Symbol "lambda", Pair(Pair(Symbol "a", Pair(Symbol "b", Symbol "c")), Pair(Symbol ""b"", Nil))) *)
let tag_parse_expression sexpr = 
   tag_parse sexpr;; 
  (* raise X_not_yet_implemented;; *)

let tag_parse_expressions sexpr = 
  List.map tag_parse_expression sexpr;;
  (* raise X_not_yet_implemented;; *)

  
end;; (* struct Tag_Parser *)
