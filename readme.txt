⟨Sexpr⟩ ::= ⟨Boolean⟩ | ⟨Char⟩ | ⟨Number⟩ | ⟨String⟩
| ⟨Symbol⟩ | ⟨List⟩ | ⟨DottedList⟩ | ⟨Quoted⟩
| ⟨QuasiQuoted⟩ | ⟨Unquoted⟩
| ⟨UnquoteAndSpliced⟩
⟨Boolean⟩ ::= #f | #t
⟨Char⟩ ::= ⟨CharPrefix⟩ ( ⟨VisibleSimpleChar⟩
| ⟨NamedChar⟩)
⟨CharPrefix⟩ ::= #\
⟨VisibleSimpleChar⟩ ::= c, where c is a character that is greater than
the space character in the ASCII table
⟨NamedChar⟩ ::= newline, nul, page, return, space, tab
⟨Number⟩ ::= ⟨Integer⟩ | ⟨Float⟩ | ⟨Fraction⟩
⟨Float⟩ ::= ⟨Integer⟩ . ⟨Natural⟩
⟨Fraction⟩ ::= ⟨Integer⟩ / ⟨Natural⟩
⟨Integer⟩ ::= (+ | -)?
⟨Natural⟩
⟨Natural⟩ ::= ⟨Digit⟩+
⟨Digit⟩ ::= (0 | · · · | 9)
⟨String⟩ ::= " ⟨StringChar⟩∗ "
⟨StringChar⟩ ::= ⟨StringLiteralChar⟩ | ⟨StringMetaChar⟩
⟨StringLiteralChar⟩ ::= c, where c is any character other than the
backslash character (\) or the double-quote
char (")
⟨StringMetaChar⟩ ::= \\ | \" | \t | \f | \n | \r
⟨Symbol⟩ ::= ⟨SymbolCharNoDot⟩ | ⟨SymbolChar⟩
⟨SymbolChar⟩+
⟨SymbolChar⟩ ::= ⟨SymbolCharNoDot⟩ | ⟨Dot⟩
⟨SymbolCharNoDot⟩ ::= (0 | · · · | 9) | (a | · · · | z) | (A | · · · | Z) | ! | $
| ^ | * | - | _ | = | + | < | > | ? | / | :
⟨Dot⟩ ::= .
⟨List⟩ ::= ( ⟨Sexpr⟩∗ )
⟨DottedList⟩ ::= ( ⟨Sexpr⟩+ . ⟨Sexpr⟩ )
⟨Quoted⟩ ::= ' ⟨Sexpr⟩
⟨QuasiQuoted⟩ ::= ` ⟨Sexpr⟩
⟨Unquoted⟩ ::= , ⟨Sexpr⟩
⟨UnquoteAndSpliced⟩ ::= ,@ ⟨Sexpr⟩





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