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
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

  let reserved_word_list =
    ["and"; "begin"; "cond"; "define"; "else";
     "if"; "lambda"; "let"; "let*"; "letrec"; "or";
     "quasiquote"; "quote"; "set!"; "unquote";
     "unquote-splicing"];;

  (* work on the tag parser starts here *)

  let list_string_to_string l =
    List.fold_left (fun acc x -> acc ^ x) "" l;;

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

  let is_nil  = function
    | Nil -> true
    | _ -> false
  let is_let_exp = function
    | Pair(Symbol("let"), args_and_vals) -> true
    | _-> false ;;

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

    | Pair(Symbol "let", Pair(args, body))-> macro_let args body

    | Pair (Symbol("let*"), Pair(args, body)) -> macro_let_star args body

    | Pair (Symbol("letrec"), Pair(args, body)) -> macro_letrec args body

    | Pair (Symbol("cond"),ribs) -> macro_cond ribs

    | Pair(Symbol "pset!", sexpr) -> macro_pset sexpr

    (* Aplic *)
    | Pair(proc, args) ->
      Applic(tag_parse proc,List.map tag_parse (scheme_list_to_ocaml_list args))

    | _ -> raise X_syntax_error

  and macro_cond ribs =


    let rec parse_cond ribs_2 =
      match ribs with
      | Nil -> Const(Void)


      | Pair(Pair(a_sexp,Pair(Symbol("=>"),Pair(b_sexp,Nil))),Nil) ->
        let lambdaSimple_if = LambdaSimple (["value"; "f"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Const Void)) in
        Applic(lambdaSimple_if,[tag_parse a_sexp]@ [LambdaSimple([],tag_parse b_sexp)]  @[])

      | Pair(Pair(a_sexp,Pair(Symbol("=>"),Pair(b_sexp,Nil))), rest_ribs) ->
        let lambdaSimple_if = LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))) in
        Applic(lambdaSimple_if,[tag_parse a_sexp]@ [LambdaSimple([],tag_parse b_sexp)]  @[LambdaSimple([],macro_cond rest_ribs)])

      | Pair(Pair(Symbol("else"),rib),_) -> body_to_expr( List.map tag_parse (scheme_list_to_ocaml_list rib))
      | Pair(Pair(test,dit),dif) -> If(tag_parse test ,body_to_expr( List.map tag_parse (scheme_list_to_ocaml_list dit)), macro_cond dif)
      | _-> raise X_syntax_error in

    parse_cond ribs


  and macro_let args body =

    let rec make_lambda_vars args_2 =
      match args_2 with
      | Nil -> Nil
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), Nil) -> Pair(Symbol(first_var), Nil)
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), rest) -> Pair(Symbol(first_var),make_lambda_vars rest)
      | _-> raise X_syntax_error in

    let rec make_lambda_vals args_3 =
      match args_3 with
      | Nil -> Nil
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), Nil) -> Pair(first_val, Nil)
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), rest) -> Pair(first_val,make_lambda_vals rest)
      | _-> raise X_syntax_error in

    let lambda_vars = make_lambda_vars args in
    let lambda_vals = make_lambda_vals args in
    let applic_vals = List.map tag_parse (scheme_list_to_ocaml_list lambda_vals) in

    let lambda = tag_parse (Pair(Symbol("lambda"),Pair(lambda_vars, body))) in
    Applic(lambda, applic_vals)

  and macro_let_star args body =

    let rec let_star_tag_parse =
      match args with
      | Nil -> macro_let args body
      | Pair(Pair(Symbol(fisrt_var), Pair(sexp,Nil)), Nil)->  macro_let args body
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), rest) ->
        Applic(LambdaSimple([first_var],(tag_parse(Pair(Symbol("let*"),Pair(rest, body))))), [tag_parse first_val])
      | _-> raise X_syntax_error in
    let_star_tag_parse


  and macro_letrec args body =

    let rec make_lambda_vars args_1 =
      match args_1 with
      | Nil -> Nil
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), Nil) -> Pair(Symbol(first_var), Nil)
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), rest) -> Pair(Symbol(first_var),make_lambda_vars rest)
      | _-> raise X_syntax_error in

    let rec make_lambda_vals args_3 =
      match args_3 with
      | Nil -> Nil
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), Nil) -> Pair(first_val, Nil)
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), rest) -> Pair(first_val,make_lambda_vals rest)
      | _-> raise X_syntax_error in

    let rec make_sets_as_exp_list args_4 =
      match args_4 with
      | Nil-> []
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), Nil)-> [Set(Var(first_var),tag_parse first_val)]
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), rest) -> [Set(Var(first_var),tag_parse first_val)]@(make_sets_as_exp_list rest)
      | _ -> raise X_syntax_error in

    let rec make_whatever args_4 =
      match args_4 with
      | Nil-> Nil
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), Nil) -> Pair(Pair(Symbol(first_var), Pair(Symbol("whatever"),Nil)), Nil)
      | Pair(Pair(Symbol(first_var), Pair(first_val,Nil)), rest) ->Pair(Pair(Symbol(first_var), Pair(Symbol("whatever"),Nil)),make_whatever rest)
      | _ -> raise X_syntax_error in

    let letrec_vars = make_lambda_vars args in
    let letrec_vars_as_exp_list  = List.map (function | Symbol(x) -> x | _-> raise X_syntax_error) (scheme_list_to_ocaml_list letrec_vars) in
    (*let letrec_vals = make_lambda_vals args in*)
    let whatever_list =scheme_list_to_ocaml_list( make_lambda_vals(make_whatever args)) in
    let whatever_list_as_exp = List.map (function | Symbol(x) -> Const (Sexpr (Symbol x)) | _-> raise X_syntax_error) whatever_list in
    let applic_no_vars_body_ = (body_to_expr (List.map tag_parse (scheme_list_to_ocaml_list body))) in
    let applic_no_vars = Applic(LambdaSimple([], applic_no_vars_body_),[]) in
    let sets_list = make_sets_as_exp_list args in
    Applic(LambdaSimple(letrec_vars_as_exp_list, (body_to_expr(sets_list @ [applic_no_vars]))),whatever_list_as_exp)


  and macro_QQ = function
    | Pair(Symbol "unquote",Pair(a,Nil)) -> (tag_parse a)
    | Pair(Symbol "unquote-splicing", Pair(a,Nil)) -> (tag_parse a)
    | Pair(Pair (Symbol "unquote-splicing",Pair(sexpr,Nil)),b) ->
      Applic(Var "append",([tag_parse sexpr;(macro_QQ b)]))
    | Pair(Pair (Symbol "unquote", Pair (a, Nil)),b) ->
      Applic(Var "cons",([tag_parse a;(macro_QQ b)]))
    | Pair(Pair(a,b),c) ->
      Applic(Var "cons",([(macro_QQ (Pair(a,b)));(macro_QQ c)]))
    | Pair(a,b) ->
      Applic(Var "cons",([tag_parse (wrap_with_quote a);(macro_QQ b)]))
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
    let combineAllVarsNames = list_string_to_string (List.map (function | Var(x) -> x | _ -> raise X_syntax_error) vars_scheme_list) in
    let lambda_arglist = List.map (function | Var(x) -> Var(x ^ (String.make 1 '_')^combineAllVarsNames) | _ -> raise X_syntax_error) vars_scheme_list  in
    let lambda_inner_body =
      List.map (function (a,b) -> Set(a,b)) (zip vars_scheme_list lambda_arglist) in

    (* let lambda_inner_body = lambda_inner_body @ [(Const(Sexpr(Bool false)))] in *)
    (* create the expr: (not send it to tag_parse recursively)*)
    (*reanme expr_i to expr_i_i *)
    let lambda_arglist_str = List.map (function | Var(x) -> (x ^ (String.make 1 '_')^combineAllVarsNames) | _ -> raise X_syntax_error) vars_scheme_list in
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
