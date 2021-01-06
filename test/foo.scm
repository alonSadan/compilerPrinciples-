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



; (define my_plus2
;     (lambda(x) (lambda () (+ x 1))))
; ((my_plus2 1 ))

; (define my_plus3 (lambda (z) (lambda (x y) (+ x y z))))
; ((my_plus3 4) 5 6)

; (((lambda (x) (lambda (y) (+ x y))) 4) 5)
; (define x (lambda () (lambda() (lambda() #t))))
; (((x)))

; (define fact
;         (lambda (n)
;           (if (= n 0) 1 (* n (fact (+ n -1))))))
; (fact 5)

; (define foo (lambda (x) (lambda (y) (lambda (z) (+ z x)))))
; (((foo 4) 5) 6)

; (define x '(1 2))
; (set-cdr! x 5)
; x
; (apply + '(4 5))


; (apply cons 1 '(2))


; (define fib
;  (lambda (n)
;   (if (< n 2)
;   1
;   (+ (fib (+ n -1)) (fib (+ n -2))))))
; (fib 5)


; ; (cons* 1 2 3 4)

; (define my_plus
;     (lambda (x y) (+ x y)))

; (apply my_plus 1 '(2))

; (apply (lambda (x y) (+ x y)) 3 2 '(4))


; (define map
;   (let ((null? null?)
; 	(car car) (cdr cdr)
; 	(cons cons) (apply apply))
;   (letrec ((map-many
; 	    (lambda (f lists)
; 	      (if (null? (car lists))
; 		  '()
; 		  (cons
; 		   (apply f (map-one car lists))
; 		   (map-many f (map-one cdr lists))))))
; 	   (map-one
; 	    (lambda (f s)
; 	      (if (null? s)
; 		  '()
; 		  (cons (f (car s))
; 			(map-one f (cdr s)))))))
;     (lambda (f . args)
;       (map-many f args)))))

; (map + '(1 2) '(3 4))

(define map-one 
  (lambda (f s) 
    (if (null? s) '() 
      (cons 
        (f (car s))
        (map-one f (cdr s))))))


(define map-many
	(lambda (f lists)
	  (if (null? (car lists))
		  '()
		  (cons
		   (apply f (map-one car lists))
		   (map-many f (map-one cdr lists))))))

(define map (lambda (f . args) (map-many f args)))

(map + '(1 2) '(3 4))

; ((lambda (x y) ((lambda (x)  x) x)) 2 3)

; ((lambda ()
;     ((lambda (a b c d e) e)
;         'a 'b 'c 'd 'e)))