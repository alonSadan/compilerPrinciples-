
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
  
  let nt_boolean = pack (disj (make_spaced (word_ci "#t")) (make_spaced (word_ci "#f")))
    (function 
    | ['#'; 't'] -> Bool(true)
    | ['#'; 'T'] -> Bool(true)
    | ['#'; 'f'] -> Bool(false)
    | ['#'; 'F'] -> Bool(false)
    | _ -> raise X_no_match);;
  
  let int_of_list ds = int_of_string(list_to_string ds);;
  let digit = range '0' '9';;
  let digits = plus digit;;

  let tok_int = caten 
                (maybe (disj (make_left_spaced (char '+')) (make_left_spaced (char '-'))))
                digits;;

  let nt_int = pack tok_int
    (function
      |(Some(sign),ds) -> Fraction(int_of_list (sign::ds),1)
      |(_,ds) -> Fraction(int_of_list ds,1));;
  
      let rec gcd1 a b =
        match b with 
          | 0 -> a
          | b -> gcd1 b (a mod b)

  let nt_fraction = 
    let rec gcd a b =
      match b with 
        | 0 -> a
        | b -> gcd b (a mod b) in
    let frac = caten (caten tok_int (char '/')) digits in 
    let frac = pack frac 
        (function 
         | (((Some(sign),a),_),b) -> (int_of_list (sign::a),int_of_list b) 
         | (((_,a),_),b) -> (int_of_list a,int_of_list b)) in
      pack frac (function (a,b) -> 
                  if a < 0 then Fraction(-1 * (a / (gcd a b)),-1 * (b / (gcd a b))) 
                  else Fraction(a / (gcd a b),b / (gcd a b)));; 

  let float_of_list ds = float_of_string(list_to_string ds);;
  let nt_float = 
    let fl = pack (caten tok_int (caten (char '.') digits))
            (function 
              | ((Some(sign),numberA),(point,numberB)) -> (sign::numberA)@[point]@numberB
              | ((_,numberA),(point,numberB)) -> numberA@[point]@numberB) in
          pack fl (function (e)->Float(float_of_list e));;

  
  let nt_number = pack (disj_list [nt_fraction;nt_int;nt_float]) 
                  (function (e) -> Number(e));;

  let letter_lowercase = range 'a' 'z';;
  let letter_uppercase = pack (range 'A' 'Z') (function (e) -> lowercase_ascii e);;
  (* map char string: *)
  let punctuation = one_of "!$^*-_=+<>/?";; 
  
  let tok_symbol_char = disj_list [digit;letter_lowercase;letter_uppercase;punctuation];;
  let tok_char_not_dot = pack (plus (char '.'))
                            (function (e) -> 
                              if List.length e = 1 then raise X_no_match
                              else e);; 
                              
  (* ⟨SymbolCharNoDot⟩ | ⟨SymbolChar⟩+ *)
  let nt_symbols = pack (disj tok_char_not_dot (plus tok_symbol_char))
                          (function (e)-> Symbol(list_to_string e));;
                    
  
  (* ⟨String⟩ ::= " ⟨StringChar⟩∗ " *)
  (* ⟨StringChar⟩ ::= ⟨StringLiteralChar⟩ | ⟨StringMetaChar⟩ *)
  (* ⟨StringLiteralChar⟩ ::= c, where c is any character other than the
    backslash character (\) or the double-quote
    char 
   *)

  
  (* \f is not  a valid special meta char in Ocaml - so we need to parse it differently*)
  let meta_chars = one_of  "\r\n\t\\\"";;
  let tok_meta_chars = pack (plus (disj
                                    (pack (word "\\f") (function (e) -> list_to_string e))                                  
                                    (pack meta_chars (function (e) ->String.make 1 e))                                                                
                                  )
                            )(function (e) -> String(String.concat "" e));


                                            
      

    
  (* let named_chars = List.map char_of_int [0;10;13;9;12;32];;
  let tok_named_chars = pack (disj_list (List.map char meta_chars))
                            (function (e) -> String.make 1 e); 
   *)
                          
end;;

open Tester;;