Question regarding tests:
1)
156c156
< - : sexpr list = [Symbol "1a^"]
---
> - : sexpr list = [Number (Fraction (1, 1)); Symbol "a^"]

3)







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