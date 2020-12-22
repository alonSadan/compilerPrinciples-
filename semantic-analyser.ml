#use "tag-parser.ml";;

type var =
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | Box'(VarFree v1), Box'(VarFree v2) -> String.equal v1 v2
  | Box'(VarParam (v1,mn1)), Box'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Box'(VarBound (v1,mj1,mn1)), Box'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxGet'(VarFree v1), BoxGet'(VarFree v2) -> String.equal v1 v2
  | BoxGet'(VarParam (v1,mn1)), BoxGet'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | BoxGet'(VarBound (v1,mj1,mn1)), BoxGet'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxSet'(VarFree v1,e1), BoxSet'(VarFree v2, e2) -> String.equal v1 v2 && (expr'_eq e1 e2)
  | BoxSet'(VarParam (v1,mn1), e1), BoxSet'(VarParam (v2,mn2),e2) -> String.equal v1 v2 && mn1 = mn2 && (expr'_eq e1 e2)
  | BoxSet'(VarBound (v1,mj1,mn1),e1), BoxSet'(VarBound (v2,mj2,mn2),e2) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2 && (expr'_eq e1 e2)
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                            (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
    | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
    | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
    (List.for_all2 String.equal vars1 vars2) &&
    (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
    (String.equal var1 var2) &&
    (List.for_all2 String.equal vars1 vars2) &&
    (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
    (expr'_eq e1 e2) &&
    (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;

exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct

  exception Itay_X_Expr of expr;;
  exception Itay_X_Expr' of expr';;
  exception Itay_X_string_list_list of string list list;;

  let rec find_exist_item item lst =
    match lst with
    | [] -> -1
    | hd::tl -> if hd = item then 0 else 1 + find_exist_item item tl;;

  let find item lst =
    match (List.mem item lst) with
    | true -> find_exist_item item lst
    | false -> -1;;

  let annotate_lexical_var var env =
    let rec env_major_minor item lst major_acc =
      (* assuming lst is without last item in origin lst and also is reversed *)
      match lst with
      | [] -> major_acc,-1
      | _ ->
        let minor = find item (List.hd lst) in
        if minor >= 0 then major_acc,minor
        else env_major_minor item (List.tl lst) (major_acc+1) in
    let name = match var with
      | Var(s) -> s
      | _ -> raise X_syntax_error in
    match env with
    | [] -> VarFree(name)
    | lst ->
      let index = find name (List.hd env) in
      (
        if index >= 0 then VarParam(name,index)
        else
          let major,minor = env_major_minor name (List.tl env) 0 in
          if minor = -1 then VarFree(name)
          else VarBound(name,major,minor)
      );;

  (* let annotate_lexical_var var env = VarFree("not implmented yet");; *)


  (* env should be list of list vars/string where the the item_i represent the level i in the current scope *)
  (* ToDo: maybe declare env type  *)
  let rec annotate_lexical_expr expr env =
    match expr with
    | Const(e) -> Const'(e)
    | Var(e) -> Var'(annotate_lexical_var expr env)
    | If(test,dit,dif) -> If'(annotate_lexical_expr test env,annotate_lexical_expr dit env,annotate_lexical_expr dif env)
    | Seq(exprs) ->  Seq'(List.map (fun e -> annotate_lexical_expr e env) exprs)
    | Set(a,b) -> Set'(annotate_lexical_var a env,annotate_lexical_expr b env)
    | Def(name,value) -> Def'(annotate_lexical_var name env,annotate_lexical_expr value env)
    | Or(exprs) ->  Or'(List.map (fun e -> annotate_lexical_expr e env) exprs)
    | LambdaSimple(arglist,body) -> LambdaSimple'(arglist, annotate_lexical_expr body ([arglist] @ env))
    | LambdaOpt(arglist,opt,body) -> LambdaOpt'(arglist,opt,annotate_lexical_expr body ([arglist @ [opt]] @ env))
    | Applic(proc,args) -> Applic'(annotate_lexical_expr proc env,List.map (fun e -> annotate_lexical_expr e env) args);;

  let rec annotate_tail expr' isTP =
    match expr',isTP with
    | Var'(expr'),_ -> Var'(expr')
    | Const'(expr'),_ -> Const'(expr')

    | Or'(exprs),_ ->
      (match exprs with
       | [] -> Or'[]
       | lst ->
         let lst = List.rev lst in
         let exprs2 = (annotate_tail (List.hd lst) isTP)::List.map (fun e -> annotate_tail e false) (List.tl lst) in
         Or'(List.rev exprs2))

    | Seq'(exprs),_ ->
      (match exprs with
       | [] -> Seq'[]
       | lst ->
         let lst = List.rev lst in
         let exprs2 = (annotate_tail (List.hd lst) isTP)::List.map (fun e -> annotate_tail e false) (List.tl lst) in
         Seq'(List.rev exprs2))

    | If'(test,dit,dif),_ -> If'(annotate_tail test false,annotate_tail dit isTP,annotate_tail dif isTP)
    | Def'(var',expr),_ -> Def'(var',annotate_tail expr false)
    | Set'(var',expr),_ -> Set'(var',annotate_tail expr false)

    | LambdaSimple'(arglist,body),_ -> LambdaSimple'(arglist, annotate_tail body true)
    | LambdaOpt'(arglist,opt,body),_ -> LambdaOpt'(arglist, opt,annotate_tail body true)

    | Applic'(proc,args),true -> ApplicTP'(annotate_tail proc false,List.map (fun e -> annotate_tail e false) args)
    | Applic'(proc,args),false -> Applic'(annotate_tail proc false,List.map (fun e -> annotate_tail e false) args)

    | _ -> raise (Itay_X_Expr' expr')


  let rec get_reads_closure expr var_str closuresList curr_closure=
    match expr with
    | Const'(e) -> closuresList
    | Var'(v) ->
      (match v with
       | VarFree(x) -> if x = var_str then closuresList @ curr_closure else closuresList
       | VarParam(x,_) -> if x = var_str then closuresList @ curr_closure else closuresList
       | VarBound(x,_,_) -> if x = var_str then closuresList @ curr_closure else closuresList)
    | If'(test,dit,dif) ->
      (
        let ifClosures = List.map (fun e -> (get_reads_closure e var_str closuresList curr_closure)) [test;dit;dif] in
        List.fold_left (fun acc a -> acc @ a) closuresList ifClosures
      )
    | Seq'(exprs) ->
      (
        let seqClosures = List.map (fun e -> (get_reads_closure e var_str closuresList curr_closure)) exprs in
        List.fold_left (fun acc a -> acc @ a) closuresList seqClosures
      )

    | Set'(a,b) -> get_reads_closure b var_str closuresList curr_closure
    | Def'(name,value) -> get_reads_closure value var_str closuresList curr_closure
    | Or'(exprs) ->   let seqClosures = List.map (fun e -> (get_reads_closure e var_str closuresList curr_closure)) exprs in
      List.fold_left (fun acc a -> acc @ a) closuresList seqClosures
    | LambdaSimple'(arglist,body) ->
      (
        get_reads_closure body var_str closuresList [(LambdaSimple'(arglist,body))]
      )
    | LambdaOpt'(arglist,opt,body) ->
      (
        get_reads_closure body var_str closuresList [(LambdaOpt'(arglist,opt,body))]
      )
    | Applic'(proc,args) ->
      (
        let procClosure = get_reads_closure proc var_str closuresList curr_closure in
        let seqClosures = List.map (fun e -> (get_reads_closure e var_str closuresList curr_closure)) args in
        let seqClosures2 =  List.fold_left (fun acc a -> acc @ a) closuresList seqClosures in
        procClosure @ seqClosures2
      )
    | ApplicTP'(proc,args) ->
      (
        let procClosure = get_reads_closure proc var_str closuresList curr_closure in
        let seqClosures = List.map (fun e -> (get_reads_closure e var_str closuresList curr_closure)) args in
        let seqClosures2 =  List.fold_left (fun acc a -> acc @ a) closuresList seqClosures in
        procClosure @ seqClosures2
      )


    | _ -> raise X_syntax_error;;


  let rec get_writes_closure expr var_str closuresList curr_closure=
    match expr with
    | Const'(e) -> closuresList
    | Var'(v) -> closuresList
    | If'(test,dit,dif) ->
      (
        let ifClosures = List.map (fun e -> (get_writes_closure e var_str closuresList curr_closure)) [test;dit;dif] in
        List.fold_left (fun acc a -> acc @ a) closuresList ifClosures
      )
    | Seq'(exprs) ->
      (
        let seqClosures = List.map (fun e -> (get_writes_closure e var_str closuresList curr_closure)) exprs in
        List.fold_left (fun acc a -> acc @ a) closuresList seqClosures
      )

    | Set'(a,b) ->
      (match a with
       | VarFree(x) -> if x = var_str then closuresList @ curr_closure else closuresList
       | VarParam(x,_) -> if x = var_str then closuresList @ curr_closure else closuresList
       | VarBound(x,_,_) -> if x = var_str then closuresList @ curr_closure else closuresList)

    | Def'(name,value) -> get_writes_closure value var_str closuresList curr_closure
    | Or'(exprs) ->   let seqClosures = List.map (fun e -> (get_writes_closure e var_str closuresList curr_closure)) exprs in
      List.fold_left (fun acc a -> acc @ a) closuresList seqClosures
    | LambdaSimple'(arglist,body) ->
      (
        get_writes_closure body var_str closuresList [(LambdaSimple'(arglist,body))]
      )
    | LambdaOpt'(arglist,opt,body) ->
      (
        get_writes_closure body var_str closuresList [(LambdaOpt'(arglist,opt,body))]
      )
    | Applic'(proc,args) ->
      (
        let procClosure = get_writes_closure proc var_str closuresList curr_closure in
        let seqClosures = List.map (fun e -> (get_writes_closure e var_str closuresList curr_closure)) args in
        let seqClosures2 =  List.fold_left (fun acc a -> acc @ a) closuresList seqClosures in
        procClosure @ seqClosures2
      )
    | ApplicTP'(proc,args) ->
      (
        let procClosure = get_writes_closure proc var_str closuresList curr_closure in
        let seqClosures = List.map (fun e -> (get_writes_closure e var_str closuresList curr_closure)) args in
        let seqClosures2 =  List.fold_left (fun acc a -> acc @ a) closuresList seqClosures in
        procClosure @ seqClosures2
      )


    | _ -> raise X_syntax_error;;


  let rec get_read_writes_closure expr var_str readClosuresList writeClosuresList curr_closure=
    match expr with
    | Const'(e) -> readClosuresList,writeClosuresList
    | Var'(v) ->
      (match v with
       | VarFree(x) -> if x = var_str then readClosuresList @ curr_closure,writeClosuresList else readClosuresList,writeClosuresList
       | VarParam(x,_) -> if x = var_str then readClosuresList @ curr_closure,writeClosuresList else readClosuresList,writeClosuresList
       | VarBound(x,_,_) -> if x = var_str then readClosuresList @ curr_closure,writeClosuresList else readClosuresList,writeClosuresList)
    | If'(test,dit,dif) ->
      (
        let ifClosures = List.map (fun e -> (get_read_writes_closure e var_str readClosuresList writeClosuresList curr_closure)) [test;dit;dif] in
        let a = List.fold_left (fun acc a -> acc @ a) readClosuresList (List.map (fun x -> fst x) ifClosures)  in
        let b = List.fold_left (fun acc b -> acc @ b) writeClosuresList (List.map (fun x -> snd x) ifClosures) in
        a,b
      )
    | Seq'(exprs) ->
      (
        let seqClosures = List.map (fun e -> (get_read_writes_closure e var_str readClosuresList writeClosuresList curr_closure)) exprs in
        let a = List.fold_left (fun acc a -> acc @ a) readClosuresList (List.map (fun x -> fst x) seqClosures)  in
        let b = List.fold_left (fun acc b -> acc @ b) writeClosuresList (List.map (fun x -> snd x) seqClosures) in
        a,b
      )

    | Set'(a,b) ->
      ( let x =
          (match a with
           | VarFree(x) -> if x = var_str then readClosuresList,writeClosuresList @ curr_closure else readClosuresList,writeClosuresList
           | VarParam(x,_) -> if x = var_str then readClosuresList,writeClosuresList @ curr_closure else readClosuresList,writeClosuresList
           | VarBound(x,_,_) -> if x = var_str then readClosuresList,writeClosuresList @ curr_closure else readClosuresList,writeClosuresList) in
        let y = get_read_writes_closure b var_str readClosuresList writeClosuresList curr_closure in
        (fst x @ fst y),(snd x @ snd y)
      )

    | Def'(name,value) ->
      ( let x =
          (match name with
           | VarFree(v) -> if v = var_str then readClosuresList,writeClosuresList @ curr_closure else readClosuresList,writeClosuresList
           | VarParam(v,_) -> if v = var_str then readClosuresList,writeClosuresList @ curr_closure else readClosuresList,writeClosuresList
           | VarBound(v,_,_) -> if v = var_str then readClosuresList,writeClosuresList @ curr_closure else readClosuresList,writeClosuresList) in
        let y = get_read_writes_closure value var_str readClosuresList writeClosuresList curr_closure in
        (fst x @ fst y),(snd x @ snd y)
      )
    | Or'(exprs) ->
      (
        let orClosures = List.map (fun e -> (get_read_writes_closure e var_str readClosuresList writeClosuresList curr_closure)) exprs in
        let a = List.fold_left (fun acc a -> acc @ a) readClosuresList (List.map (fun x -> fst x) orClosures)  in
        let b = List.fold_left (fun acc b -> acc @ b) writeClosuresList (List.map (fun x -> snd x) orClosures) in
        a,b
      )

    | LambdaSimple'(arglist,body) ->
      (
        get_read_writes_closure body var_str readClosuresList writeClosuresList [(LambdaSimple'(arglist,body))]
      )
    | LambdaOpt'(arglist,opt,body) ->
      (
        get_read_writes_closure body var_str readClosuresList writeClosuresList [(LambdaOpt'(arglist,opt,body))]
      )
    | Applic'(proc,args) ->
      (
        let procClosure = get_read_writes_closure proc var_str readClosuresList writeClosuresList curr_closure in
        let argsClosures = List.map (fun e -> (get_read_writes_closure e var_str readClosuresList writeClosuresList curr_closure)) args in
        let a = List.fold_left (fun acc a -> acc @ a) readClosuresList (List.map (fun x -> fst x) argsClosures)  in
        let b = List.fold_left (fun acc b -> acc @ b) writeClosuresList (List.map (fun x -> snd x) argsClosures) in
        (fst procClosure @ a),(snd procClosure @ b)
      )
    | ApplicTP'(proc,args) ->
      (
        let procClosure = get_read_writes_closure proc var_str readClosuresList writeClosuresList curr_closure in
        let argsClosures = List.map (fun e -> (get_read_writes_closure e var_str readClosuresList writeClosuresList curr_closure)) args in
        let a = List.fold_left (fun acc a -> acc @ a) readClosuresList (List.map (fun x -> fst x) argsClosures)  in
        let b = List.fold_left (fun acc b -> acc @ b) writeClosuresList (List.map (fun x -> snd x) argsClosures) in
        (fst procClosure @ a),(snd procClosure @ b)
      )


    | _ -> raise X_syntax_error;;

  let rec get_var_lst exp var_name =
    match exp with
    | Const'(e) -> []
    | Var'(name) -> (match name with
        | VarFree(v) -> if v = var_name then [VarFree(v)] else []
        | VarParam(v,major) -> if v = var_name then [VarParam(v,major)] else []
        | VarBound(v,major,minor) -> if v = var_name then [VarBound(v,major,minor)] else [])


    | If'(test,dit,dif) ->
      (
        let vars = List.map (fun e -> (get_var_lst e var_name)) [test;dit;dif] in
        List.fold_left (fun acc a -> acc @ a) [] vars
      )
    | Seq'(exprs) | Or'(exprs) ->
      (
        let vars = List.map (fun e -> (get_var_lst e var_name)) exprs in
        List.fold_left (fun acc a -> acc @ a) [] vars
      )

    | Set'(name,value) |  Def'(name,value)->
      (let x =
         (match name with
          | VarFree(v) -> if v = var_name then [VarFree(v)] else []
          | VarParam(v,major) -> if v = var_name then [VarParam(v,major)] else []
          | VarBound(v,major,minor) -> if v = var_name then [VarBound(v,major,minor)] else []) in
       let y = get_var_lst value var_name in
       x @ y
      )

    | LambdaSimple'(arglist, body)  ->
      (
        get_var_lst body var_name
      )
    | LambdaOpt'(arglist,opt,body)  ->
      (
        get_var_lst body var_name
      )
    | Applic'(proc,args) | ApplicTP'(proc,args) ->
      (
        let procvars = get_var_lst proc var_name  in
        let vars = List.map (fun e -> (get_var_lst e var_name)) args in
        let vars2 =  List.fold_left (fun acc a -> acc @ a) [] vars in
        procvars @ vars2
      )
    | _ -> raise X_syntax_error;;

  let rec get_seq_list exp var_name seq_lst curr_item_in_seq =
    match exp with
    | Const'(e) -> []
    | Var'(name) -> (match name with
        | VarFree(v) -> if v = var_name then seq_lst @ curr_item_in_seq else seq_lst
        | VarParam(v,major) -> if v = var_name then seq_lst @ curr_item_in_seq else seq_lst
        | VarBound(v,major,minor) -> if v = var_name then seq_lst @ curr_item_in_seq else seq_lst)

    | If'(test,dit,dif) ->
      (
        let seqs = List.map (fun e -> (get_seq_list e var_name seq_lst curr_item_in_seq)) [test;dit;dif] in
        List.fold_left (fun acc a -> acc @ a) [] seqs
      )
    | Seq'(exprs)->
      (
        let seqs = List.map (fun e -> (get_seq_list e var_name seq_lst [e])) exprs in
        [Seq'(List.fold_left (fun acc a -> acc @ a) [] seqs)]
      )
    | Or'(exprs) ->
      (
        let seqs = List.map (fun e -> (get_seq_list e var_name seq_lst curr_item_in_seq)) exprs in
        List.fold_left (fun acc a -> acc @ a) [] seqs
      )
    | Set'(name,value) |  Def'(name,value)->
      (let x =
         (match name with
          | VarFree(v) -> if v = var_name then curr_item_in_seq else []
          | VarParam(v,major) -> if v = var_name then curr_item_in_seq else []
          | VarBound(v,major,minor) -> if v = var_name then curr_item_in_seq else []) in
       let y = get_seq_list value var_name seq_lst curr_item_in_seq in
       x @ y
      )

    | LambdaSimple'(arglist, body)  ->
      (
        get_seq_list body var_name seq_lst curr_item_in_seq
      )
    | LambdaOpt'(arglist,opt,body)  ->
      (
        get_seq_list body var_name seq_lst curr_item_in_seq
      )
    | Applic'(proc,args) | ApplicTP'(proc,args) ->
      (
        let procseqs = get_seq_list proc var_name seq_lst curr_item_in_seq  in
        let seqs = List.map (fun e -> (get_seq_list e var_name seq_lst curr_item_in_seq)) args in
        let seqs2 =  List.fold_left (fun acc a -> acc @ a) [] seqs in
        procseqs @ seqs2
      )
    | _ -> raise X_syntax_error;;


  let should_box expr name =
    let isExistBound0 v = List.exists (fun x-> match x with | VarBound(_,0,_) -> true | _ -> false) v in
    let isExistBound0Plus v = List.exists (fun x-> match x with | VarBound(_,major,_) -> major > 0 | _ -> false) v in
    let isExistParam v = List.exists (fun x -> match x with VarParam(_,_)-> true |_-> false) v in
    let rec checkVarFromEnvsHelper r w =
      (if not (r == w) then
         (
           let v1 = get_var_lst r name in
           let v2 = get_var_lst w name in
           if v1 = [] || v2 = [] then false
           else (
             if isExistBound0(v1) && isExistBound0(v2) then true
             else if isExistParam(v1) && isExistBound0Plus(v2) then true
             else if isExistBound0Plus(v1) && isExistParam(v2) then true
             else false
           )
         )
       else false)
    in

    let rec isNotSameRibs readlst writelst name =
      (match readlst,writelst with
       | [],_ -> false
       | _,[] -> false
       | r::rs,w::ws ->
         let car =
           (
             if (checkVarFromEnvsHelper r w) = true then true else isNotSameRibs (r::rs) ws name
           ) in
         if car = true then true else isNotSameRibs rs (w::ws) name) in

    let readlst,writelst = get_read_writes_closure expr name [] [] [] in
    if (isNotSameRibs readlst writelst name) = false then false
    else(
      let seq_lst = get_seq_list expr name [] [] in
      let rec  criteria1 = function
        | [] -> false
        | e::es ->
          (match e with
           | Set'(var,_) ->
             (match var with
              | VarFree(v) -> if v = name then not ((List.fold_left (fun acc e_es -> acc @ (get_reads_closure e_es name [] [])) es []) = []) else false
              | VarParam(v,major) -> if v = name then not ((List.fold_left (fun acc e_es -> acc @ (get_reads_closure e_es name [] [])) es []) = []) else false
              | VarBound(v,major,minor) -> if v = name then not ((List.fold_left (fun acc e_es -> acc @ (get_reads_closure e_es name [] [])) es []) = []) else false)
           | _ -> criteria1 es
          ) in
      let rec  criteria2 = function
        | [] -> false
        | e::es ->
          (match e with
           | Var'(var) ->
             (match var with
              | VarFree(v) -> if v = name then not ((List.fold_left (fun acc e_es -> acc @ (get_writes_closure e_es name [] [])) es []) = []) else false
              | VarParam(v,major) -> if v = name then not((List.fold_left (fun acc e_es -> acc @ (get_writes_closure e_es name [] [])) es []) = []) else false
              | VarBound(v,major,minor) -> if v = name then not((List.fold_left (fun acc e_es -> acc @ (get_writes_closure e_es name [] [])) es []) = []) else false)


           | _ -> criteria2 es
          ) in
      let rec extra_res = function
        | [] -> true
        | Seq'(seq_body)::es -> not ((criteria1 seq_body) || (criteria2 seq_body)) && (extra_res es)
        | _ -> true

      in
      extra_res seq_lst
    )


  let rec flatten_expr'_seq'_body = function
    | (Seq'(e)::es) -> e @ (flatten_expr'_seq'_body es)
    | (e::es) -> e :: (flatten_expr'_seq'_body es)
    | [] -> [];;

  let rec body_to_expr body =
    if (List.length body) > 1 then Seq'(flatten_expr'_seq'_body body)
    else if (List.length body) = 1 then (List.hd body)
    else raise X_syntax_error;;


  let rec make_box expr box_vars =
    match expr with
    | Const'(e) -> Const'(e)
    | Var'(v) ->
      (match v with
       | VarFree(x) -> Var'(v)
       | VarParam(x,minor) -> if List.mem x box_vars then BoxGet'(VarParam(x,minor)) else expr
       | VarBound(x,minor,major) -> if List.mem x box_vars then BoxGet'(VarBound(x,minor,major)) else expr)

    | If'(test,dit,dif) -> If'(make_box test box_vars,make_box dit box_vars, make_box dif box_vars)
    | Seq'(exprs) -> Seq'(List.map (fun e -> make_box e box_vars) exprs )
    | Set'(name,value) ->
      (match name with
       | VarFree(x) -> Set'(name,make_box value box_vars)
       | VarParam(x,minor) -> if List.mem x box_vars then BoxSet'(VarParam(x,minor), make_box value box_vars) else Set'(name,make_box value box_vars)
       | VarBound(x,minor,major) -> if List.mem x box_vars then BoxSet'(VarBound(x,minor,major), make_box value box_vars) else Set'(name,make_box value box_vars))

    | Def'(name,value) -> Def'(name, make_box value box_vars)
    | Or'(exprs) ->   Or'(List.map (fun e -> make_box e box_vars) exprs )

    | LambdaSimple'(arglist,body) ->
      (
        let box_vars_new = List.filter (fun v-> should_box (LambdaSimple'(arglist,body)) v) arglist in
        let box_signatures = List.map (fun v-> Set'(VarParam(v, find v arglist), Box'(VarParam(v,find v arglist)))) box_vars_new in

        let box_vars2 =
          (List.fold_left
             (fun acc b ->
                (
                  if (List.mem b arglist) && (List.mem b box_vars) && (not (List.mem b box_vars_new)) then acc
                  else acc @ [b]
                )
             )
             []
             box_vars) in

        let body_box = body_to_expr (box_signatures @ [make_box body (box_vars2 @ box_vars_new)]) in
        LambdaSimple'(arglist, body_box)
      )

    | LambdaOpt'(arglist,opt,body) ->
      (
        let box_vars_new = List.filter (fun v-> should_box (LambdaOpt'(arglist,opt,body)) v) (arglist@[opt]) in
        let box_signatures = List.map (fun v-> Set'(VarParam(v, find v (arglist @ [opt])), Box'(VarParam(v,find v (arglist @ [opt])))))  box_vars_new in

        let box_vars2 =
          (List.fold_left
             (fun acc b ->
                (
                  if (List.mem b (arglist @ [opt])) && (List.mem b box_vars) && (not (List.mem b box_vars_new)) then acc
                  else acc @ [b]
                )
             )
             []
             box_vars) in

        let body_box = body_to_expr (box_signatures @ [make_box body (box_vars2 @ box_vars_new)])  in
        LambdaOpt'(arglist, opt, body_box)
      )

    | Applic'(proc,args) ->
      ( let new_args = List.map (fun arg -> make_box arg box_vars) args in
        let new_proc = make_box proc box_vars in
        Applic'(new_proc, new_args)
      )

    | ApplicTP'(proc,args) ->
      ( let new_args = List.map (fun arg -> make_box arg box_vars) args in
        let new_proc = make_box proc box_vars in
        ApplicTP'(new_proc, new_args)
      )


    | _ -> raise X_syntax_error;;

  let annotate_lexical_addresses e = annotate_lexical_expr e [];;

  let annotate_tail_calls e = annotate_tail e false;;
  (*raise X_not_yet_implemented;;*)

  let box_set e = make_box e [];;
  (* raise X_not_yet_implemented;; *)

  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)


