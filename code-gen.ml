#use "semantic-analyser.ml";;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second
     argument is the fvars table type, and the third is an expr' that has been annotated
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;
(*1 . remove duplicates
2. sort topologically*)

let rec make_set = function
  | [] -> []
  | hd :: tl ->
    if List.mem hd tl then make_set tl
    else  hd::(make_set tl);;

let rec andmap f s =
  match s with
  | [] -> true
  | car :: cdr -> (f car) && (andmap f cdr)

let rec scheme_list_to_ocaml_list = function
  | Pair(a,b) -> a::(scheme_list_to_ocaml_list b)
  | s -> [s];;

let is_in_lst item lst =
  match item,lst with
    | Pair(a1,b1),Pair(a2,b2) -> (
        let lst1 = scheme_list_to_ocaml_list (Pair(a1,b1)) in
        let lst2 = scheme_list_to_ocaml_list (Pair(a2,b2)) in
          andmap (fun a -> List.mem a lst1) lst2
    )
    | x,Pair(a,b) -> (
      let lst = scheme_list_to_ocaml_list (Pair(a,b)) in
        List.mem x lst
    )
    | _,_ -> false;;


let rec toplogy_sort  = function
  | [] -> []
  | hd :: tl -> (
      let new_car =
        List.fold_left
          (fun acc a -> match is_in_lst a hd with | true -> a :: acc | false -> acc)
          [hd]
          tl in
      let new_cdr =
        List.fold_left
          (fun acc a -> match is_in_lst a hd with | true ->  acc | false -> a::acc)
          []
          tl in

      (toplogy_sort new_car) @ (toplogy_sort new_cdr)

      (*TODO: check if nested sub list should be supported*)
      (* | _ ->  hd :: (toplogy_sort tl)                              *)
  );;

let rec make_const_table asts pos table =
  match asts with
   | [] -> table (* )constant * int * string) list *)
   | c::cs ->
      match c with
      (* recursive call args:
        - rest of the consts
        - calc position for next entry
        - string representation of const
      *)
        | Char(ch) -> make_const_table cs (pos + 2) table @ [Sexpr(c) ,(pos,"T_CHAR \n" )]
        | Number(n) ->
          (match n with
            | Float(f) -> make_const_table cs (pos + 9) table @ [Sexpr(c) ,(pos,"T_FLOAT \n" )]
            | Fraction(num,den) -> make_const_table cs (pos + 9) table @ [Sexpr(c) ,(pos,"T_RATIONAL \n" )])
        | String (s) -> make_const_table cs (pos+17) table @ [Sexpr(c), (pos, "T_STRING \n")]
        | _ -> raise X_not_yet_implemented;;
     (*maybe add NIL and VOID*)

let rec make_naive_sexpr_list asts ans =
  match asts with
   | [] -> []
   | hd :: tl -> (
     let curr =
      match hd with
        | Const'(c) -> (
            match c with
            | Sexpr(s) -> ans @ [s]
            | Void -> ans
          )
        | If'(test,dit,dif) -> make_naive_sexpr_list [test;dit;dif] ans
        | Seq'(l) | Or'(l) -> make_naive_sexpr_list l ans
        | Set'(var,e) | Def'(var,e) -> make_naive_sexpr_list [e] ans
        | LambdaSimple'(args,body) -> make_naive_sexpr_list [body] ans
        | LambdaOpt'(args,opt,body) -> make_naive_sexpr_list [body] ans
        | Applic'(proc,args) | ApplicTP'(proc,args) -> make_naive_sexpr_list ([proc] @ args) ans
        | _ -> ans in
     make_naive_sexpr_list tl curr)

let updated_asts asts =
  let naive_lst = make_naive_sexpr_list asts [] in
  let set = make_set naive_lst in
    toplogy_sort set;;

module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = make_const_table (updated_asts asts) 0 [];;
  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;

(*type expr' =
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
  | ApplicTP' of expr' * (expr' list);;*)

  (*type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;*)


