#use "semantic-analyser.ml";;

module type GENSYM =
   sig
     val next : string -> string
end ;;

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

exception ALON_ERR1 of expr';;

let primitive_names =
  ["boolean?"; "flonum?"; "rational?"; "pair?"; "null?"; "char?"; "string?";                                                                                                                              "procedure?"; "symbol?"; "string-length"; "string-ref"; "string-set!";
 "make-string"; "symbol->string"; "char->integer"; "integer->char";
 "exact->inexact"; "eq?"; "+"; "*"; "/"; "="; "<"; "numerator"; "denominator";
 "gcd"];;


let rec make_set = function
  | [] -> []
  | hd :: tl ->
    if List.mem hd tl then make_set tl
    else  hd::(make_set tl);;

let toplogy_sort lst =
  let rec sub_lists = function
    | Pair(a,b) -> a :: (sub_lists b) @ [b]
    | Symbol(s) -> String(s) :: [(Symbol(s))]
    | s -> [s] in

  (* List.fold_left (fun acc x -> acc @ (sub_lists x) @ [x] ) [] lst;; *)
  List.fold_right (fun x acc -> acc @ (sub_lists x) @ [x] ) lst [];;

let get_pos constant table = fst (List.assoc constant table);;
let get_str_pos constant table = string_of_int (fst (List.assoc constant table));;

let str_comment_pos pos = "\t\t;;" ^ string_of_int pos

let rec make_const_table asts pos table =
  match asts with
   | [] -> table (* )constant * int * string) list *)
   | hd::cs ->
      match hd with
        | Void -> make_const_table cs (pos + 1) (table @ [Void ,(pos,"MAKE_VOID " ^ str_comment_pos pos)])
        | Sexpr(c) -> (
          match c with
            | Char(ch) -> make_const_table cs (pos + 2) (table @ [Sexpr(c) ,(pos,"MAKE_LITERAL_CHAR(" ^ string_of_int (int_of_char ch) ^ ")" ^ str_comment_pos pos)])
            | Bool(b) ->
              (match b with
                | true -> make_const_table cs (pos + 2) (table @ [Sexpr(c) ,(pos,"MAKE_BOOL(1)" ^  str_comment_pos pos)])
                | false -> make_const_table cs (pos + 2) (table @ [Sexpr(c) ,(pos,"MAKE_BOOL(0)"  ^ str_comment_pos pos)]))
            | Number(n) ->
              (match n with
                | Float(f) -> make_const_table cs (pos + 9) (table @ [Sexpr(c) ,(pos,"MAKE_LITERAL_FLOAT("^ string_of_float f ^")"  ^ str_comment_pos pos)])
                | Fraction(num,den) -> make_const_table cs (pos + 17) (table @ [Sexpr(c) ,(pos,"MAKE_LITERAL_RATIONAL("^string_of_int num^","^string_of_int den^")" ^ str_comment_pos pos)]))
            | String (s) -> make_const_table cs (pos+9+String.length s) (table @ [Sexpr(c), (pos, "MAKE_LITERAL_STRING \"" ^ s ^"\"" ^ str_comment_pos pos)])
            | Nil -> make_const_table cs (pos + 1) (table @ [Sexpr(c) ,(pos,"MAKE_NIL" ^ str_comment_pos pos)])
            | Symbol(s) -> make_const_table cs (pos + 9)
                (table @ [Sexpr(c) ,(pos,"MAKE_LITERAL_SYMBOL(const_tbl+"^get_str_pos (Sexpr(String s)) table^")" ^ str_comment_pos pos)])
                (* (table) *)
            | Pair(a,b) -> make_const_table cs (pos + 17)
                (table @ [Sexpr(c) ,(pos,"MAKE_LITERAL_PAIR(const_tbl+"^get_str_pos (Sexpr(a)) table^",const_tbl+"^get_str_pos (Sexpr(b)) table^")"  ^ str_comment_pos pos)]))


let rec make_fvars_table asts pos table =
  match asts with
   | [] -> table (* (string * int) list *)
   | fv::fvars ->
      make_fvars_table fvars (pos + 8) (table @ [(fv,pos)])
    (* make_fvars_table fvars (pos + 8) table @[(string_of_int pos)] *)

let rec make_first_sexpr_lst asts ans =
  match asts with
   | [] -> ans
   | hd :: tl -> (
     let curr =
      match hd with
        | Const'(c) -> (
            match c with
            | Sexpr(s) -> ans @ [s]
            | Void -> ans
          )
        | If'(test,dit,dif) -> make_first_sexpr_lst [test;dit;dif] ans
        | Seq'(l) | Or'(l) -> make_first_sexpr_lst l ans
        | Set'(var,e) | Def'(var,e) -> make_first_sexpr_lst [e] ans
        | LambdaSimple'(args,body) -> make_first_sexpr_lst [body] ans
        | LambdaOpt'(args,opt,body) -> make_first_sexpr_lst [body] ans
        | Applic'(proc,args) | ApplicTP'(proc,args) -> make_first_sexpr_lst ([proc] @ args) ans
        | _ -> ans in
     make_first_sexpr_lst tl curr);;

let rec make_naive_fvar_lst asts ans =
  match asts with
   | [] -> ans
   | hd :: tl -> (
     let curr =
      match hd with
        | Var'(v) ->(
          match v with
            | VarFree(name) -> ans @ [name]
            | _ -> ans
        )
        | If'(test,dit,dif) -> make_naive_fvar_lst [test;dit;dif] ans
        | Seq'(l) | Or'(l) -> make_naive_fvar_lst l ans
        | Set'(var,e) | Def'(var,e) -> make_naive_fvar_lst [e] (make_naive_fvar_lst [(Var'(var))] ans)
        | LambdaSimple'(args,body) -> make_naive_fvar_lst [body] ans
        | LambdaOpt'(args,opt,body) -> make_naive_fvar_lst [body] ans
        | Applic'(proc,args) | ApplicTP'(proc,args) -> make_naive_fvar_lst ([proc] @ args) ans
        | _ -> ans in
        make_naive_fvar_lst tl curr);;

let init_const_tbl_lst asts =
  let naive_lst = make_first_sexpr_lst asts [] in
  let set = make_set naive_lst in
  let sort_set =  toplogy_sort set in
    make_set (
      [Void;Sexpr Nil;Sexpr (Bool false); Sexpr (Bool true)] @
      (List.map (fun e -> Sexpr(e)) sort_set));;    (*TODO check if we remove duplicates from the end of the list*)

let make_fvars_table_helper asts =
  let naive_fvar_lst = make_naive_fvar_lst asts [] in
  let set = make_set naive_fvar_lst @ primitive_names in
    set;;


let get_fvar_index name table = List.assoc name table;;
let get_str_fvar_index name table = string_of_int (List.assoc name table);;


module Gensym : GENSYM =
   struct
     let c = ref 0
     let next s = incr c ; s ^ (string_of_int !c)
  end;;



let rec make_generate constant_table fvars_table e =
  match e with
    | Const'(c) -> make_gen_const constant_table c
    | Var'(v) -> (
      match v with
        | VarFree(name) -> make_gen_free_var fvars_table name
        | _ -> ""
    )
    | Seq'(exprs) -> String.concat "\n" (List.map (make_generate constant_table fvars_table) exprs)
    | If'(test,dit,dif) -> make_gen_if constant_table fvars_table test dit dif
    | Or'(exprs) -> make_gen_or constant_table fvars_table exprs
    | Def'(var, value) -> make_gen_set constant_table fvars_table var value
    | _ -> ""
      (* raise (ALON_ERR1 e) *)
and make_gen_const constant_table c = "mov rax, const_tbl+" ^ (get_str_pos c constant_table)
(* "const_tbl+" ^ (get_str_pos c constant_table) *)
and make_gen_free_var fvars_table name = "mov rax, qword fvar_tbl+" ^ (get_str_fvar_index name fvars_table)

and make_gen_if constant_table fvars_table test dit dif =
  let ind = (Gensym.next "") in
  (make_generate constant_table fvars_table test) ^ " \n" ^
  "cmp rax, SOB_FALSE_ADDRESS \n
    je Lelse" ^ ind ^ " \n" ^
  (make_generate constant_table fvars_table dit) ^ " \n" ^
  "jmp Lexit"^ind^" \n" ^
  "Lelse"^ind^ ": \n" ^
  (make_generate constant_table fvars_table dif) ^ "\n" ^
   "Lexit"^ind^": \n"

and make_gen_or constant_table fvars_table exprs =
  let ind = (Gensym.next "") in
  let eps_lst = List.map (make_generate constant_table fvars_table) exprs in
  let ans =
   String.concat "\n"
    (List.map (fun e-> e ^ "\n" ^
      "cmp rax, SOB_FALSE_ADDRESS\n
      jne Lexit"^ind ^"\n" ) eps_lst) in
      ans ^ "Lexit"^ind^":"
(*not importantkasjndkasd akjsdbakdsjb*)
and make_gen_set constant_table fvars_table var value =
  let eps = make_generate constant_table fvars_table value in
  match var with
    | VarFree(name) ->
        eps ^ "\n" ^
        "mov [fvar_tbl+" ^ (get_str_fvar_index name fvars_table) ^ "],rax\n
         mov rax, SOB_VOID_ADDRESS"
    | _ -> "";;

module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = make_const_table (init_const_tbl_lst asts) 0 [];;
  let make_fvars_tbl asts = make_fvars_table (make_fvars_table_helper asts) 0 [];;
  let generate consts fvars e = make_generate consts fvars e;;
end;;



