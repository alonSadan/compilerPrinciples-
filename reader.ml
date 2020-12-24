
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

  let nt_whitespaces = star nt_whitespace;;

  let make_paired nt_left nt_right nt =
    let nt = caten nt_left nt in
    let nt = pack nt (function (_, e) -> e) in
    let nt = caten nt nt_right in
    let nt = pack nt (function (e, _) -> e) in
    nt;;

  let make_spaced nt = make_paired nt_whitespaces nt_whitespaces nt;;
  let make_left_spaced nt = make_paired nt_whitespaces nt_epsilon nt;;
  let make_right_spaced nt = make_paired nt_epsilon nt_whitespaces nt;;

  (* (pack (char '\n') (function (_) -> [])) *)
  let nt_end_of_line = (pack (char '\n') (function (_) -> []));;

  let tok_semicolun = make_spaced ( char ';');;

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

  let tok_int =
    let nt =caten
        (maybe (disj (make_left_spaced (char '+')) (make_left_spaced (char '-'))))
        digits
    in make_left_spaced nt;;

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
    let abs = fun x -> if x < 0 then -x else x in
    let frac = caten (caten tok_int (char '/')) digits in
    let frac = make_spaced frac in
    let frac = pack frac
        (function
          | (((Some(sign),a),_),b) -> (int_of_list (sign::a),int_of_list b)
          | (((_,a),_),b) -> (int_of_list a,int_of_list b)) in
    pack frac (function (a,b) ->
        let g = abs (gcd a b) in
        Fraction(a/g, b/g));;

  let float_of_list ds = float_of_string(list_to_string ds);;
  let nt_float =
    let fl = caten tok_int (caten (char '.') digits) in
    let fl = pack (make_spaced fl)
        (function
          | ((Some(sign),numberA),(point,numberB)) -> (sign::numberA)@[point]@numberB
          | ((_,numberA),(point,numberB)) -> numberA@[point]@numberB) in
    pack fl (function (e)->Float(float_of_list e));;


  let nt_number = pack (disj_list [nt_fraction;nt_float;nt_int]) (function (e) -> Number(e));;


  let nt_number_scientific_notation =
    let nt = caten (disj nt_float nt_int) (caten (char_ci 'e') nt_int) in
    let nt = pack nt
        (function
          | (Fraction(e1,_),(_,Float(e2))) -> raise X_no_match
          | (Float(e1),(_,Float(e2))) -> raise X_no_match
          | (Float(n1) , (_,Fraction(n2,_))) -> Number(Float(n1*.(10.0**float_of_int(n2))))
          | (Fraction(n1,_) , (_,Fraction(n2,_))) -> Number(Float(float_of_int(n1)*.(10.0**float_of_int(n2)))))
    in (disj nt nt_number) ;;

  let letter_lowercase = range 'a' 'z';;
  let letter_uppercase = pack (range 'A' 'Z') (function (e) -> lowercase_ascii e);;
  (* map char string: *)
  let punctuation = one_of "!$^*-_=+<>/?:";;
  let tok_dot = char '.';;
  let tok_char_not_dot = disj_list [digit;letter_lowercase;letter_uppercase;punctuation];;
  let tok_symbol_char = disj tok_char_not_dot tok_dot

  (* ⟨Symbol⟩ ::= ⟨SymbolCharNoDot⟩ | ⟨SymbolChar⟩⟨SymbolChar⟩+ *)
  let nt_symbols =
    let nt1 = pack tok_char_not_dot (function (e) -> [e]) in
    let nt2 = pack (caten tok_symbol_char (plus tok_symbol_char)) (function (ch,ch_list) -> [ch]@ch_list) in
    let nt = pack (disj nt2 nt1)
        (function (e)-> Symbol(list_to_string e)) in
    make_spaced nt;;


  let nt_char =
    let nt_named_chars = disj_list (List.map word_ci
                                      ["#\\nul";"#\\newline";"#\\return";"#\\tab";"#\\page";"#\\space"]) in

    let nt_named_chars = pack (make_spaced nt_named_chars) list_to_string in
    let nt_named_chars = pack nt_named_chars
        (function (res) -> match (String.lowercase_ascii res) with
            | "#\\nul" -> char_of_int 0
            | "#\\newline" -> char_of_int 10
            | "#\\return" -> char_of_int 13
            | "#\\tab" -> char_of_int 9
            | "#\\page" -> char_of_int 12
            | "#\\space" -> char_of_int 32
            | _ -> raise  X_no_match
        ) in
    let nt_visble_char = const (fun ch -> ch > ' ') in
    let nt_visble_char = pack (make_spaced (caten (make_left_spaced (word "#\\")) nt_visble_char))
        (function (_,e) -> e) in
    pack (disj nt_named_chars nt_visble_char)
      (function (e) -> Char(e));;

  let nt_literal_char = const (fun ch -> ch != '\"' && ch != '\\'  && ch >= ' ');;
  (* \f is not  a valid special meta char in Ocaml - so we need to parse it differently*)
  let nt_meta_char =
    let meta_chars = one_of  "\\\r\n\t" in
    let meta_chars_pair = pack (caten (char '\\') (one_of "rntf\"\\"))
        (function
          | (_,'r') -> char_of_int 13
          | (_,'n') -> char_of_int 10
          | (_,'t') -> char_of_int 9
          | (_,'f') -> char_of_int 12
          | (_,'\"') -> char_of_int 34
          | (_,'\\') -> char_of_int 92
          | _ -> raise X_no_match) in
    disj meta_chars_pair meta_chars;;

  (* String -> "(StringliteralChar | StringMetaChar)* "  *)
  (* ToDo: nt_string should work without removing (quote) from string meta char *)
  let nt_string =
    let nt_left_string_quote = (make_left_spaced (char '\"')) in
    let nt_right_string_quote =  (make_right_spaced (char '\"')) in
    let nt_string_char = star (disj nt_literal_char nt_meta_char) in
    let nt  = pack (caten nt_left_string_quote (caten nt_string_char nt_right_string_quote))
        (function (quote_start,(body,quote_end)) -> String (list_to_string body)) in
    nt;;

  let nt_line_comment=
    let nt_end_of_comment = disj nt_end_of_line nt_end_of_input in
    (* let nt_line_comment = not_followed_by nt_any nt_end_of_comment in *)
    let nt_comment_body = star (const (fun ch -> ch != '\n')) in
    let nt_line_comment = caten  (make_left_spaced (char ';')) (caten nt_comment_body nt_end_of_comment) in
    pack nt_line_comment (fun e -> "Comment")


  let lParen = (make_spaced (char '('));;
  let rParen = (make_spaced (char ')'));;
  let dot = (make_spaced (char '.'));;

  let rec nt_list_pair s=
    let nt = star nt_sexpr in
    let packed =  pack (caten lParen (caten nt rParen))
        (function
            (_, (sexp_list, _)) ->
            List.fold_right
              (fun a b -> Pair(a,b))
              sexp_list
              Nil) in
    packed s

  (* ToDo: update pair the will work with nested () *)
  and nt_dotted_list_pair s=
    let nt = caten (plus nt_sexpr) (caten dot nt_sexpr) in
    let packed =  pack (caten lParen (caten nt rParen))
        (function
            (_,((sexpr_list , (_ , sexpr)) , _)) ->
            List.fold_right
              (fun a b ->   Pair(a,b))
              sexpr_list
              sexpr) in
    packed s

  and nt_make_quote nt_char str s=
    let nt_q = caten nt_char nt_sexpr in
    let packed = pack nt_q (function (_,sexpr) ->
        Pair(Symbol(str),Pair(sexpr,Nil))) in
    packed s

  and nt_make_quote2 nt_word str s=
    let nt_q = caten nt_word nt_sexpr in
    let packed = pack nt_q (function(_,sexpr) ->
        Pair(Symbol(str),Pair(sexpr,Nil))) in
    packed s

  and nt_quoted s = nt_make_quote (make_spaced (char '\'')) "quote" s
  and nt_qquoted s = nt_make_quote (make_spaced (char '`')) "quasiquote" s
  and nt_unquoted s = nt_make_quote (make_spaced (char ',')) "unquote" s
  and nt_unquoted_spliced s= nt_make_quote2 (make_spaced (word ",@")) "unquote-splicing" s

  (* ⟨Sexpr⟩ ::= ⟨Boolean⟩ | ⟨Char⟩ | ⟨Number⟩ | ⟨String⟩
     | ⟨Symbol⟩ | ⟨List⟩ | ⟨DottedList⟩ | ⟨Quoted⟩
     | ⟨QuasiQuoted⟩ | ⟨Unquoted⟩
     | ⟨UnquoteAndSpliced⟩ *)

  (*    S -> #;(S | nt_epsilon)sexpr    *)
  and nt_sexpr_comment s=
    let nt =
      fun x ->
        pack (caten (make_spaced (word "#;"))
                (caten (disj nt_sexpr_comment (pack nt_epsilon (fun _ -> "Comment"))) nt_sexpr))
          (fun _ -> "Comment")
          x in
    nt s


  and nt_comment s= (disj nt_line_comment nt_sexpr_comment) s

  and nt_nil s=
    let packed = pack (caten lParen (caten (star nt_comment) rParen))
        (function _ -> Nil) in
    packed s

  and nt_sexpr s=
    let (_,s) = ((star nt_comment) s) in
    let nt_number_not_followed_symbol =
      not_followed_by nt_number_scientific_notation
        (disj_list [letter_lowercase;letter_uppercase;punctuation]) in
    let (e1,s) =
      (
        disj_list [

          nt_number_not_followed_symbol;
          (* nt_boolean_not_followed_symbol; *)
          nt_boolean;
          (* nt_number_scientific_notation; *)
          nt_symbols;

          nt_char;
          nt_string;nt_list_pair;nt_dotted_list_pair;
          nt_quoted;nt_qquoted;nt_unquoted;nt_unquoted_spliced;nt_nil] s
      ) in
    let (e2, s) = (star nt_comment) s in
    (e1,s);;

  let read_sexprs string =
    let nt =  (star nt_sexpr)  in
    let (e,s) = nt (string_to_list string) in
    let (_,s2) = nt_whitespaces s in
    match e,s,s2 with
    | e,_,[] -> e
    | _ -> raise X_no_match

  (* ToDo: check if Case sensiticty is already implemented*)

end;;

open Reader;;
