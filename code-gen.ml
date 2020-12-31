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
   "gcd";"car";"cdr";"set-car!";"set-cdr!"];;


let rec make_set = function
  | [] -> []
  | hd :: tl ->
    if List.mem hd tl then make_set tl
    else  hd::(make_set tl);;


let rec make_set_sexpr = function
  | [] -> []
  | hd :: tl ->
    if List.exists (fun x -> sexpr_eq hd x) tl then make_set_sexpr tl
    else  hd::(make_set_sexpr tl);;

let rec make_set_constant = function
  | [] -> []
  | hd :: tl ->
    if List.exists
        (function
          | Void -> (
              match hd with
              | Void -> true
              | Sexpr(_) -> false)
          | Sexpr(x) -> (
              match hd with
              | Void -> false
              | Sexpr(y) -> sexpr_eq x y)
        ) tl then make_set_constant tl
    else  hd::(make_set_constant tl);;


let make_set_constant_wrapper lst = List.rev (make_set_constant (List.rev lst))

let toplogy_sort lst =
  let rec sub_lists = function
    | Pair(a,b) -> (sub_lists a) @ [a] @ (sub_lists b) @ [b]
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
        | String (s) -> make_const_table cs (pos+9+ (String.length s)) (table @ [Sexpr(c), (pos, "MAKE_LITERAL_STRING \"" ^ s ^"\"" ^ str_comment_pos pos)])
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

let rec make_init_fvar_lst asts ans =
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
        | If'(test,dit,dif) -> make_init_fvar_lst [test;dit;dif] ans
        | Seq'(l) | Or'(l) -> make_init_fvar_lst l ans
        | Set'(var,e) | Def'(var,e) -> make_init_fvar_lst [e] (make_init_fvar_lst [(Var'(var))] ans)
        | LambdaSimple'(args,body) -> make_init_fvar_lst [body] ans
        | LambdaOpt'(args,opt,body) -> make_init_fvar_lst [body] ans
        | Applic'(proc,args) | ApplicTP'(proc,args) -> make_init_fvar_lst ([proc] @ args) ans
        | _ -> ans in
      make_init_fvar_lst tl curr);;

let init_const_tbl_lst asts =
  let naive_lst = make_first_sexpr_lst asts [] in
  let set = make_set_sexpr naive_lst in
  let sort_set =  toplogy_sort set in
  make_set_constant_wrapper (
    [Void;Sexpr Nil;Sexpr (Bool false); Sexpr (Bool true)] @
    (List.map (fun e -> Sexpr(e)) sort_set));;    (*TODO check if we remove duplicates from the end of the list*)

let make_fvars_table_helper asts =
  let naive_fvar_lst = make_init_fvar_lst asts [] in
  let set = (make_set primitive_names) @ naive_fvar_lst in
  set;;

let get_fvar_index name table = List.assoc name table;;
let get_str_fvar_index name table = string_of_int (List.assoc name table);;

module Gensym : GENSYM =
struct
  let c = ref 0
  let next s = incr c ; s ^ (string_of_int !c)
end;;

let make_comment str num = "\t\t;;" ^ str ^ string_of_int num

let rec make_generate  constant_table fvars_table e=
  match e with
  | Const'(c) -> make_gen_const constant_table c
  | Var'(v) -> make_gen_var fvars_table v
  | Seq'(exprs) -> String.concat "\n" (List.map (make_generate  constant_table fvars_table) exprs)
  | If'(test,dit,dif) -> make_gen_if  constant_table fvars_table test dit dif
  | Or'(exprs) -> make_gen_or  constant_table fvars_table exprs
  | Def'(var,value) | Set'(var,value) -> make_gen_set  constant_table fvars_table var value
  | BoxGet'(var) -> make_gen_box_get constant_table fvars_table var
  | BoxSet'(var,value) -> make_gen_box_set  constant_table fvars_table var value
  | LambdaSimple'(arglist,body) -> make_gen_lambda  constant_table fvars_table arglist body
  | Applic'(proc,args) -> make_gen_applic  constant_table fvars_table proc args
  | _ -> ""
(* raise (ALON_ERR1 e) *)
and make_gen_const constant_table c = "mov rax, const_tbl+" ^ (get_str_pos c constant_table) ^ "\n"
and make_gen_var fvars_table v =
  match v with
  | VarFree(name) -> "mov rax, qword [fvar_tbl+" ^ get_str_fvar_index name fvars_table ^ "] \n "
  | VarParam(_, minor) -> "mov rax, qword [rbp + " ^ (string_of_int (8*(4+minor))) ^"] " ^make_comment "pvar: " minor^" \n"
  | VarBound(_, major, minor) ->
    "mov rax, qword [rbp + 16] \n
        mov rax, qword [rax + " ^ (string_of_int (8*major)) ^"]
        mov rax, qword [rax + " ^ (string_of_int (8*minor)) ^"] \n"

and make_gen_if  constant_table fvars_table test dit dif=
  let ind = (Gensym.next "") in
  (make_generate  constant_table fvars_table test) ^ " \n" ^
  "cmp rax, SOB_FALSE_ADDRESS \n
    je Lelse" ^ ind ^ " \n" ^
  (make_generate  constant_table fvars_table dit) ^ " \n" ^
  "jmp Lexit"^ind^" \n" ^
  "Lelse"^ind^ ": \n" ^
  (make_generate  constant_table fvars_table dif) ^ "\n" ^
  "Lexit"^ind^": \n"

and make_gen_or  constant_table fvars_table exprs=
  let ind = (Gensym.next "") in
  let eps_lst = List.map (make_generate  constant_table fvars_table) exprs in
  let ans =
    String.concat "\n"
      (List.map (fun e-> e ^ "\n" ^
                         "cmp rax, SOB_FALSE_ADDRESS\n
      jne Lexit"^ind ^"\n" ) eps_lst) in
  ans ^ "Lexit"^ind^":"

and make_gen_set  constant_table fvars_table var value=
  let eps = make_generate  constant_table fvars_table value in
  match var with
  | VarFree(name) ->
    eps ^
    "mov [fvar_tbl+" ^ (get_str_fvar_index name fvars_table) ^ "],rax\n
         mov rax, SOB_VOID_ADDRESS"
  | VarParam(_, minor)->
    eps ^
    "mov qword [rbp + " ^ (string_of_int (8*(4+minor))) ^"], rax \n
        mov rax, SOB_VOID_ADDRESS \n"
  | VarBound(_,major,minor) ->
    eps ^
    "mov rbx, qword [rbp + 16] \n
        mov rbx, qword [rbx + " ^ (string_of_int (8*major)) ^"] \n
        mov qword [rbx + " ^ (string_of_int (8*minor)) ^"], rax \n
        mov rax, SOB_VOID_ADDRESS \n"

and make_gen_box_get constant_table fvars_table e =
  let eps = make_gen_var fvars_table e in
  eps ^
  "mov rax, qword [rax] \n"

and make_gen_box_set  constant_table fvars_table var value =
  let eps_var = make_gen_var fvars_table var in
  let eps = make_generate  constant_table fvars_table value in
  eps ^
  "push rax \n
    "^ eps_var ^"
    pop qword [rax] \n
    mov rax, SOB_VOID_ADDRESS \n"
and make_gen_lambda  constant_table fvars_table arglist body =
  (*only for empty lex : make closure without extend 
    ToDo: maybe also give lcode,lbody another ind (they share same instance with the IF labels)*)
    (* LEXICAL_ENV = [rbp+16] *)
  
  let ind = (Gensym.next "") in   
  (* LEXICAL_ENV = null => { void* LEXICAL_ENV = malloc(size_of word), *LEXICAL = null }   *)
  let alloc_first_env = 
    "cmp LEXICAL_ENV, SOB_NIL_ADDRESS  
    jne end_first_alloc"^ind^" \n 
    MALLOC rbx, WORD_SIZE 
    mov qword [rbx],SOB_NIL_ADDRESS
    mov LEXICAL_ENV, rbx 
    end_first_alloc"^ind^":\n" in
  
  (* calc |ENV| and store it in rcx *)
  let get_env_size =
    "mov rcx, 0
    mov rbx, LEXICAL_ENV 
    mov rcx, 0 
    l_env_counter"^ind^":
    \t cmp qword [rbx], SOB_NIL_ADDRESS 
    \t je l_env_counter_end"^ind^" 
    \t add rbx, WORD_SIZE 
    \t inc rcx 
    \t jmp l_env_counter"^ind^" 
    l_env_counter_end"^ind^":
    " in

  (* 1. alloc ext_env and store and store it in rdx
  2. store current env in rbx *)
  let allocate_ext_env = 
    "mov rbx,LEXICAL_ENV
    inc rcx 
    mov rax,rcx
    shl rax,3
    MALLOC rdx, rax 
    " in

  (* using loop opertator that works with rcx*)
  let copy_pointers = 
    "lcopy"^ind^":
    \t mov rax, qword[rbx + WORD_SIZE*rcx] 
    \t mov qword [rdx + WORD_SIZE*rcx + WORD_SIZE], rax 
    \t loop lcopy"^ind^" \n
    mov rax,qword[rbx] 
    mov qword[rdx],rax 
    " in 

  let num_of_args = "mov rbx, ARGS_NUMBER ;; get number of args \n" in
  let minors_size ="shl rbx, 3  ;;mul with size of word (array of pointers) \n" in
  let minors  = "MALLOC rcx, rbx ;; allocate minors\n" in
  let rec alloc_minors ind =
    if ind = (List.length arglist) then ""
    else 
    "mov rax,PVAR("^string_of_int ind^") \n 
    mov qword[rcx+"^string_of_int (8*ind) ^"], rax ;; copy arg from stack to mnor array in env \n"
    ^ (alloc_minors (ind + 1)) in

  let set_lex_env = 
    "mov qword [rdx], rcx ;; load minors \n
    mov LEXICAL_ENV,rdx
    " in
  let make_ext_env = 
    alloc_first_env^get_env_size^allocate_ext_env^copy_pointers^
    num_of_args^minors_size^minors^set_lex_env^(alloc_minors 0)^set_lex_env in

  let eps_body = make_generate constant_table fvars_table body in
  let code =
    "Lcode"^ind^":
    push rbp
    mov rbp , rsp \n
    ;; LAMBDA_BODY: \n"
    ^ eps_body ^ "\n
    leave
    ret \n
    Lcont"^ind^": \n" in

  ";; Lambda_Simple: \n
  Lambda_Simple"^ind^":\n
  "^make_ext_env^" \n
  jmp Lcont"^ind^ "\n"
  ^ code ^
  "mov rbx,LEXICAL_ENV \n
  MAKE_CLOSURE(rax, rbx, Lcode"^ind^" ) \n"

and make_gen_applic  constant_table fvars_table proc args =
  (* ToDo: maybe also add magic number to stack  *)
  let eps_lst = List.map (make_generate  constant_table fvars_table) (List.rev args) in
  let eps_proc = make_generate  constant_table fvars_table proc in
  let ans_args = String.concat "\n" (List.map (fun e-> e ^ "push rax \n") eps_lst) in
  ";;APPLIC: \n" ^
  ";;APPLIC_args: \n" ^
  ans_args ^ "push "^string_of_int (List.length args) ^" \n"
  ^ ";; APPLIC_proc: \n"
  ^ eps_proc ^
  "CLOSURE_ENV r8, rax ;;store closure env in r8  (pointer register)
  CLOSURE_CODE r9, rax ;;store closure code/body in r9 (pointer register)
  push r8
  call r9
  add rsp, 8 ;; pop env
  pop rbx ;; pop arg count
  shl rbx, 3 ;; rbx = rbx * 8
  add rsp, rbx ;; pop args \n"
;;


module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = make_const_table (init_const_tbl_lst asts) 0 [];;
  let make_fvars_tbl asts = make_fvars_table (make_fvars_table_helper asts) 0 [];;
  let generate consts fvars e = make_generate consts fvars e ;;
end;;



