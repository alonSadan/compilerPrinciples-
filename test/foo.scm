; #t 
; #f 

; '(1 2 3 4 5 6)
; "abc"
; 'xyz

; (begin (define x 5) x)
; (set! x 3)
; (if #f 1 x)
; (if #t (set! x 2) x)
; x

; (+ 50 50)
; (car '(7 8))
; (cdr '(7 8))

(define my_plus 
    (lambda (x y) (+ x y)))
(my_plus 1 2)

(define my_plus2 
    (lambda (x y) 
        (lambda (z) (+ x y z))))
((my_plus2 1 2) 3)
