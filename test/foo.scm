(define -
  (lambda (a b)
    (+ a (* -1 b))))

#t
#f

'(1 2 3 4 5 6)
"abc"
'xyz

(cons* 1 2 3 4)

(define my_plus
    (lambda (x y) (+ x y)))

(my_plus 1 2)
(apply my_plus 1 '(2))

(apply (lambda (x y) (+ x y)) 3 2 '(4))




(apply apply `(,apply (,apply (,my_plus (1 2 3))))) 
(apply
apply
`(,apply
(,apply
(,apply
(,apply
(,apply
(,apply
(,apply (,apply (,apply (,apply (,my_plus (1 2 3 )))))))))))))



(apply (lambda () 'ok) '())

(define foo0
    (lambda (n . s)
      (if (zero? n)
          s
          (apply foo0 (- n 1) `(ha ,@s)))))

(foo0 3)





(apply cons 1 '(2))



;--------------------------------------------------------


(fold-left + 0 '(1 2 3))
(fold-right + 0 '(1 2 3))
(reverse '(1 2 3))
(cons* 1 2 3)
(list 4 5 6)
(make-string 10 #\x)




(begin (define x 5) x)
(set! x 3)
(if #f 1 x)
(if #t (set! x 2) x)
x

(+ 50 50)
(car '(7 8))
(cdr '(7 8))



(define my_plus2
    (lambda(x) (lambda () (+ x 1))))
((my_plus2 1 ))

(define my_plus3 (lambda (z) (lambda (x y) (+ x y z))))
((my_plus3 4) 5 6)

(((lambda (x) (lambda (y) (+ x y))) 4) 5)
(define x (lambda () (lambda() (lambda() #t))))
(((x)))

(define fact
        (lambda (n)
          (if (= n 0) 1 (* n (fact (+ n -1))))))
(fact 5)

(define foo (lambda (x) (lambda (y) (lambda (z) (+ z x)))))
(((foo 4) 5) 6)

(define x '(1 2))
(set-car! x 5)
x
; (set-cdr! x 6)
; x


(define fib
 (lambda (n)
  (if (< n 2)
  1
  (+ (fib (+ n -1)) (fib (+ n -2))))))
(fib 5)

(null? '(1 2 3))
(fold-right cons 1 '(2 3))

(list? '(1 2 3))
(list? 1)

(zero? 6)
(integer? 3)

(equal? 2 3)

(define foo
  (lambda (n . s)
    (if (zero? n)
        s
        (apply foo (- n 1) `(ha ,s)))))

(foo 3)

(define funny
  (lambda (n)
    (if (zero? n)
        '()
        `(ha ,(apply cons `(ha ,(funny (- n 1))))))))

(funny 4)



; ; (define my_minus (lambda nums
; ;                        (fold-right (lambda (num delta)
; ;                                      (+ delta (* -1 num)))
; ;                          (car nums)
; ;                          (cdr nums))))


; ; (define my_plus (lambda nums
; ;                        (fold-right (lambda (num delta)
; ;                                      (+ delta (* 1 num)))
; ;                          (car nums)
; ;                          (cdr nums))))




((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s 
((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s 
((lambda s ((lambda s s) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) 'mary 'had 'a 'little 'lambda)









(define fact
  ((lambda (x) (x x))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((fact fact) (- n 1))))))))


(fact 5)

(let ((a 'a))
  (let ((b 'b) (c 'c))
    (let ()
      (let ()
        (let ((d 'd) (e 'e) (f 'f) (g 'g))
          (let ()
            (let ((h 'h) (i 'i))
              (let ((j 'j) (k 'k) (l 'l) (m 'm) (n 'n))
                (let ()
                  (let ((o 'o))
                    (let ((p 'p) (q 'q))
                      `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p))))))))))))



(let ((a 'a))
  `(ha ,(let ((b 'b) (c 'c))
           `(ha ,(let ()
                    `(ha ,(let ()
                             `(ha ,(let ((d 'd) (e 'e) (f 'f) (g 'g))
                                      `(ha ,(let ()
                                               `(ha ,(let ((h 'h) (i 'i))
                                                        `(ha ,(let ((j 'j) (k 'k) (l 'l) (m 'm) (n 'n))
                                                                 `(ha ,(let ()
                                                                          `(ha ,(let ((o 'o))
                                                                                   `(ha ,(let ((p 'p) (q 'q))
                                                                                            `(ha ,`(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p)))))))))))))))))))))))





(let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let
((d 'd) (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e
,f ,(let ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j
'j) (k 'k) (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a
,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d
,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a
,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c
,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e
,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let ((s 's) (t 't)) `(,a ,b
,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,(let ((u 'u) (v
'v) (w 'w)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s
,t ,u ,v ,w ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p
,q ,r ,s ,t ,u ,v ,w ,(let ((x 'x)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k
,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,(let ((y 'y) (z 'z)) `(,a ,b
,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,y
,z))))))))))))))))))))))))))))))))



;works
(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c)) 
    `(,a ,b ,c))))


(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c) (d 'd) (e 'e)) 
    `(,a ,b ,c ,d ,e))))




; ;works
(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c) (d 'd)) 
    `(,a ,b ,c,d  ,(let
        ((e 'e)) #t)))))


; ;works
(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c) (d 'd) (x 'x)) 
    `(,a ,b ,c ,x ,(let
        ((e 'e)) #t)))))


; ;works
(let ((a 'a) (b 'b) (x 'x)) 
  `(,a ,b ,(let ((c 'c) (d 'd)) 
    `(,a ,b ,c ,d ,x))))



;not works
(let ((a 'a) (b 'b)) 
  `(,(let ((c 'c)) 
    `(,a ,b ,c ,(let ((d 'd)) `(,a ,b ,c ,d))))))


(map + '(1 2) '(3 4))


(+ 1 2 3 4 5 )