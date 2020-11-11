

<Sexpr-comment -> 
(spaced <#;>) (<Sexpr-comment | nt_whitespaces) Sexpr

(* ⟨SymbolCharNoDot⟩ | ⟨SymbolChar⟩+ *)

(* ⟨String⟩ ::= " ⟨StringChar⟩∗ " *)
  (* ⟨StringChar⟩ ::= ⟨StringLiteralChar⟩ | ⟨StringMetaChar⟩ *)
  (* ⟨StringLiteralChar⟩ ::= c, where c is any character other than the
    backslash character (\) or the double-quote
    char (")
   *

found in comp's forum,maybe helpful for testing metachars:

   (define meta
  (letrec ((loop
            (lambda (s)
              (cond ((null? s) '())
                    ((char=? (car s) #\\) `(#\\ #\\ ,@(loop (cdr s))))
                    ((char=? (car s) #\") `(#\\ #\" ,@(loop (cdr s))))
                    ((char=? (car s) #\newline) `(#\\ #\n ,@(loop (cdr s))))
                    ((char=? (car s) #\tab) `(#\\ #\t ,@(loop (cdr s))))
                    ((char=? (car s) #\newline) `(#\\ #\n ,@(loop (cdr s))))
                    ((char=? (car s) #\page) `(#\\ #\f ,@(loop (cdr s))))
                    ((char=? (car s) #\return) `(#\\ #\r ,@(loop (cdr s))))
                    (else `(,(car s) ,@(loop (cdr s))))))))
    (lambda (string)
      (list->string
       `(#\" ,@(loop (string->list string)) #\")))))