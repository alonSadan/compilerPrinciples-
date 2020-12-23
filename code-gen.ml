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

  let fixed_asts asts = 
    let rec make_set = function
      | [] -> []
      | hd :: tl -> 
        if List.mem hd tl then make_set tl 
        else  hd::(make_set tl) in

    let rec andmap f s =
            match s with
            | [] -> true
            | car :: cdr -> (f car) && (andmap f cdr) in

    let rec improper_list_to_ocaml_list = (function
    | Pair(a,b) -> a::(improper_list_to_ocaml_list b)
    | s -> [s]) in
  
  let rec proper_list_to_ocaml_list = (function
    | Pair(a,b) -> a::(proper_list_to_ocaml_list b)
    | Nil -> []
    | _ -> raise X_no_match) in
  
  let rec is_proper_lst = (function
    | Pair(a,b) -> is_proper_lst b
    | Nil -> true
    | _ -> false ) in
  
  let rec is_improper_lst = (function
    | Pair(a,b) -> is_improper_lst b
    | Nil -> false
    | _ -> true) in
                    

  let is_sub_list lst1 lst2 = 
    andmap (fun a -> List.mem a lst1) lst2 in 
    
  let rec scheme_list_to_ocaml_list scheme_lst =
    (
      if is_proper_lst scheme_lst then proper_list_to_ocaml_list scheme_lst
      else  improper_list_to_ocaml_list scheme_lst
    ) in


  let rec toplogy_sort  = function
    | [] -> []
    | hd :: tl -> 
      ( 
        match hd with
          Pair(a, b) -> 
            (
              let sexpr_list =  (scheme_list_to_ocaml_list hd) in 
                match (List.filter (fun e ->  is_sub_list e sexpr_list) tl) with
                  | [] -> hd::(toplogy_sort tl)
                  | sub_lists -> 
                    (
                      let lst_without_sub_lists = List.filter (fun x -> not (List.mem x sub_lists)) (List.map (scheme_list_to_ocaml_list) tl) in
                        (toplogy_sort sub_lists) @ [sexpr_list] :: (toplogy_sort lst_without_sub_lists)
                    )
            )   (*TODO: check if nested sub list should be supported*) 
        | _ ->  hd :: (toplogy_sort tl)                             
      ) in

  let asts = make_set asts in 
  let asts = toplogy_sort asts in
    asts

  

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
        | Char(ch) -> make_const_table cs (pos + 2) table @ [(Sexpr(c) ,pos,"T_CHAR \n" )]
        | Number(n) ->
          (match n with 
            | Float(f) -> make_const_table cs (pos + 9) table @ [(Sexpr(c) ,pos,"T_FLOAT \n" )]
            | Fraction(num,den) -> make_const_table cs (pos + 9) table @ [(Sexpr(c) ,pos,"T_RATIONAL \n" )]
        | String (s) -> make_const_table cs (pos+17) table @ [(Sexpr(c), pos, "T_STRING \n")])
        | _ -> raise X_not_yet_implemented;;
     (*maybe add NIL and VOID*)

let rec make_sexpr_list expr'_list= 
   let lst = List.filter (function | Const'(Sexpr(e)) -> true | _ -> false) expr'_list in
    List.map (fun Const'(Sexpr(e)) -> e) lst;;
  
module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = make_const_table (fixed_asts (make_sexpr_list asts)) 0 [];;
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


