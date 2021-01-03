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

; (define my_plus
;     (lambda (x y) (+ x y)))
; (my_plus 1 2)
; (define my_plus2
;     (lambda(x) (lambda () (+ x 1))))
; ((my_plus2 1 ))

; (define my_plus3 (lambda (z) (lambda (x y) (+ x y z))))
; ((my_plus3 4) 5 6)

; (((lambda (x) (lambda (y) (+ x y))) 4) 5)
; (define x (lambda () (lambda() (lambda() #t))))
; (((x)))

(define fact
        (lambda (n)
          (if (= n 0) 1 (* n (fact (+ n -1))))))
(fact 5)

; ; (define foo (lambda (x) (lambda (y) (lambda (z) (+ z x)))))
; ; (((foo 4) 5) 6)

; (define x '(1 2))
; (set-cdr! x 5)
; x

; (apply + 1 2 3  '(5 6 7))