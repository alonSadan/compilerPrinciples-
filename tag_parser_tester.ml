#use "tag-parser.ml";;
open Tag_Parser;;

exception X_Assert_false of string;;



let rec zip paired_lists =
  match paired_lists with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1, h2)::(zip (t1, t2))
  | _, _ -> failwith "oops, the lists seems to have different lengths"
;;

let test_string_to_exprs input expected desc_error=
  Printf.printf "=======================================\nRead input:{%s}\n" input;
  let sexpr =  read_sexprs input in
  let expr_list = tag_parse_expressions sexpr in
  let zip_lists = zip (expected,expr_list) in
  let res =
    andmap
      (function (expr1,expr2) -> expr_eq expr1 expr2)
      zip_lists in
  match res with
   | true -> Printf.printf "Passed test\n"
   | false -> Printf.printf "{%s}\n" desc_error;;


(* test_string_to_exprs  *)
test_string_to_exprs "123" ([Const (Sexpr (Number (Fraction (123, 1))))]) "test1 failed - tag number";;
(* test_string_to_exprs "a" ([Const (Sexpr (Symbol "a"))]) "test2 failed - tag number";; *)
test_string_to_exprs "#f" ([Const (Sexpr (Bool false))]) "test3 failed - tag number";;
test_string_to_exprs "#\\b" ([Const (Sexpr (Char 'b'))]) "test4 failed - tag number";;

test_string_to_exprs "12 #f #\\b" ([
  Const (Sexpr (Number (Fraction (12, 1))));
  (* Const (Sexpr (Symbol "a")); *)
  Const (Sexpr (Bool false));
  Const (Sexpr (Char 'b'))
]) "test5 failed - tag number";;


test_string_to_exprs "x" ([Var("x")]) "test6 failed - var";;
test_string_to_exprs "(if #f 4 7)" ([If(Const(Sexpr(Bool(false))), Const(Sexpr(Number(Fraction(4,1)))),
Const(Sexpr(Number(Fraction(7,1)))))]) "test7 failed - if-then-else";;
test_string_to_exprs "(if #f 4)" ([If(Const(Sexpr(Bool(false))), Const(Sexpr(Number(Fraction(4,1)))),
Const(Void))]) "test7 failed - if-then-else";;
test_string_to_exprs "(lambda (a) \"b\")" ([LambdaSimple(["a"],Const (Sexpr (String "b")))]) "test8 failed - LambdaSimple";;
test_string_to_exprs "(lambda (a b) \"b\")" ([LambdaSimple(["a";"b"],Const (Sexpr (String "b")))]) "test9 failed - LambdaSimple";;

test_string_to_exprs "(lambda (a b . c) \"b\")" ([LambdaOpt(["a";"b"] ,"c",Const (Sexpr (String "b")))]) "test10 failed - LambdaOpt";;
test_string_to_exprs "(lambda (a b c . d) \"b\")" ([LambdaOpt(["a";"b";"c"] ,"d",Const (Sexpr (String "b")))]) "test11 failed - LambdaOpt";;


test_string_to_exprs "(lambda s 1)" ([LambdaOpt([],"s",Const(Sexpr(Number(Fraction(1,1)))))]) "test 12 failed lambdaOpt variadic";;


test_string_to_exprs "(+ 1 2)" ([Applic(Const(Sexpr(Symbol("+"))),
                                        [Const(Sexpr(Number(Fraction(1,1))));Const(Sexpr(Number(Fraction(2,1))))])]) "test 13 failed applic";;

test_string_to_exprs "(or #t #f)" ([Or([Const(Sexpr(Bool(true) ));Const(Sexpr(Bool(false)))])]) "test 14 failed or";; 

test_string_to_exprs "(define x #f)" ([Def(Const(Sexpr(Symbol("x"))),Const(Sexpr(Bool false)))]) "test 15 failed define";;                                      
test_string_to_exprs "(begin)" ([Const(Void)]) "test 16 failed empty seq";;                                      
test_string_to_exprs "(begin x)" ([Var("x")]) "test 17 failed single exp seq";;                                      