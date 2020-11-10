
#use "pc.ml";;
open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


let read_sexprs string = raise X_not_yet_implemented;;
  
end;; (* struct Reader *)

open Reader;;

module Tester = struct

  let nt_whitespaces = star nt_whitespace;;

  (* ToDo: update nt_char maybe (special char?) *)
  let nt_character = const (fun ch -> ch >= ' ');;
  let nt_characters = star nt_character

  let make_paired nt_left nt_right nt =
    let nt = caten nt_left nt in
    let nt = pack nt (function (_, e) -> e) in
    let nt = caten nt nt_right in
    let nt = pack nt (function (e, _) -> e) in
      nt;;

  let make_spaced nt = make_paired nt_whitespaces nt_whitespaces nt;;
  let make_left_spaced nt = make_paired nt_whitespaces nt_epsilon nt;;
  let make_right_spaced nt = make_paired nt_epsilon nt_whitespaces nt;;

  (* ToDo: maybe combine all the make_spaced into one token *)
  let tok_lparen = make_spaced ( char '(');;
  let tok_rparen = make_spaced ( char ')');;
  let tok_addop = make_spaced ( char '+');;
  let tok_mulop = make_spaced ( char '*');;
  let tok_semicolun = make_spaced ( char ';');;
  
  (* ToDo: fix this nt after String/char is completed *)
  let nt_end_of_line = nt_epsilon;;
  
  let nt_body = const (fun ch -> ch = 'b');;
  let nt_end = const (fun ch -> ch = 'e');;

  (* ToDo: change nt_whitespace and nt_end to read_sexpr  *)
  let rec nt_sexpr_comment s =    
    pack (caten 
          (make_spaced (word "#;")) 
          (caten 
            (disj (delayed (fun _ -> nt_sexpr_comment))
                  (pack nt_whitespaces (fun _ ->"")))
            
            nt_end))
        (fun _ ->"") s ;;    
            
  let nt_line_comment s =
    let nt_end_of_comment = disj nt_end_of_line nt_end_of_input in
    let nt_line_comment = make_paired tok_semicolun nt_end_of_comment nt_characters in
      pack nt_line_comment (fun _ ->"")s;;
  
  let tok_boolean = pack (disj (make_spaced (word_ci "#t")) (make_spaced (word_ci "#f")))
    (function 
    | ['#'; 't'] -> Bool(true)
    | ['#'; 'T'] -> Bool(true)
    | ['#'; 'f'] -> Bool(false)
    | ['#'; 'F'] -> Bool(false)
    | _ -> raise X_no_match);;
  
  let int_of_list ds = int_of_string(list_to_string ds);;
  let digit = range '0' '9';;
  let digits = plus digit;;

  let tok_int = pack digits
    (function (ds) -> Fraction(int_of_list ds,1));; 
  
  let tok_fractions = 
    let rec gcd a b =
      match b with 
        | 0 -> a
        | b -> gcd b (a mod b) in
    let frac = caten (caten digits (char '/')) digits in 
    let frac = pack frac (function ((a,_),b) -> (int_of_list a,int_of_list b)) in
      pack frac (function (a,b) -> Fraction(a / (gcd a b),b / (gcd a b)));; 

  let float_of_list ds = float_of_string(list_to_string ds);;
  let tok_float = 
    let fl = pack (caten (maybe (disj (make_left_spaced (char '+')) (make_left_spaced (char '-'))))
              (caten digits (caten (char '.') digits)))
            (function 
              | (Some(sign),(numberA,(point,numberB))) -> (sign::numberA)@[point]@numberB
              | (_,(numberA,(point,numberB))) -> numberA@[point]@numberB) in
          pack fl (function (e)->Float(float_of_list e));;

end;;

open Tester;;