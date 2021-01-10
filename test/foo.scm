; (define -
;   (lambda (a b)
;     (+ a (* -1 b))))

; #t
; #f

; '(1 2 3 4 5 6)
; "abc"
; 'xyz

; (cons* 1 2 3 4)

; (define my_plus
;     (lambda (x y) (+ x y)))

; (my_plus 1 2)
; (apply my_plus 1 '(2))

; (apply (lambda (x y) (+ x y)) 3 2 '(4))




; (apply apply `(,apply (,apply (,my_plus (1 2 3)))))
; (apply
; apply
; `(,apply
; (,apply
; (,apply
; (,apply
; (,apply
; (,apply
; (,apply (,apply (,apply (,apply (,my_plus (1 2 3 )))))))))))))



; (apply (lambda () 'ok) '())

; (define foo0
;     (lambda (n . s)
;       (if (zero? n)
;           s
;           (apply foo0 (- n 1) `(ha ,@s)))))

; (foo0 3)





; (apply cons 1 '(2))



; ;--------------------------------------------------------


; (fold-left + 0 '(1 2 3))
; (fold-right + 0 '(1 2 3))
; (reverse '(1 2 3))
; (cons* 1 2 3)
; (list 4 5 6)
; (make-string 10 #\x)




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
; (set-car! x 5)
; x
; ; (set-cdr! x 6)
; ; x


; (define fib
;  (lambda (n)
;   (if (< n 2)
;   1
;   (+ (fib (+ n -1)) (fib (+ n -2))))))
; (fib 5)

; (null? '(1 2 3))
; (fold-right cons 1 '(2 3))

; (list? '(1 2 3))
; (list? 1)

; (zero? 6)
; (integer? 3)

; (equal? 2 3)

; (define foo
;   (lambda (n . s)
;     (if (zero? n)
;         s
;         (apply foo (- n 1) `(ha ,s)))))

; (foo 3)

; (define funny
;   (lambda (n)
;     (if (zero? n)
;         '()
;         `(ha ,(apply cons `(ha ,(funny (- n 1))))))))

; (funny 4)



; ; ; (define my_minus (lambda nums
; ; ;                        (fold-right (lambda (num delta)
; ; ;                                      (+ delta (* -1 num)))
; ; ;                          (car nums)
; ; ;                          (cdr nums))))


; ; ; (define my_plus (lambda nums
; ; ;                        (fold-right (lambda (num delta)
; ; ;                                      (+ delta (* 1 num)))
; ; ;                          (car nums)
; ; ;                          (cdr nums))))




; ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s
; ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s
; ((lambda s ((lambda s s) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) 'mary 'had 'a 'little 'lambda)









; (define fact
;   ((lambda (x) (x x))
;    (lambda (fact)
;      (lambda (n)
;        (if (zero? n)
;            1
;            (* n ((fact fact) (- n 1))))))))


; (fact 5)

; (let ((a 'a))
;   (let ((b 'b) (c 'c))
;     (let ()
;       (let ()
;         (let ((d 'd) (e 'e) (f 'f) (g 'g))
;           (let ()
;             (let ((h 'h) (i 'i))
;               (let ((j 'j) (k 'k) (l 'l) (m 'm) (n 'n))
;                 (let ()
;                   (let ((o 'o))
;                     (let ((p 'p) (q 'q))
;                       `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p))))))))))))



; (let ((a 'a))
;   `(ha ,(let ((b 'b) (c 'c))
;            `(ha ,(let ()
;                     `(ha ,(let ()
;                              `(ha ,(let ((d 'd) (e 'e) (f 'f) (g 'g))
;                                       `(ha ,(let ()
;                                                `(ha ,(let ((h 'h) (i 'i))
;                                                         `(ha ,(let ((j 'j) (k 'k) (l 'l) (m 'm) (n 'n))
;                                                                  `(ha ,(let ()
;                                                                           `(ha ,(let ((o 'o))
;                                                                                    `(ha ,(let ((p 'p) (q 'q))
;                                                                                             `(ha ,`(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p)))))))))))))))))))))))





; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let
; ((d 'd) (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e
; ,f ,(let ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j
; 'j) (k 'k) (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a
; ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a
; ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c
; ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e
; ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let ((s 's) (t 't)) `(,a ,b
; ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,(let ((u 'u) (v
; 'v) (w 'w)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s
; ,t ,u ,v ,w ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p
; ,q ,r ,s ,t ,u ,v ,w ,(let ((x 'x)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k
; ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,(let ((y 'y) (z 'z)) `(,a ,b
; ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,y
; ,z))))))))))))))))))))))))))))))))



; ;works
; (let ((a 'a) (b 'b))
;   `(,a ,b ,(let ((c 'c))
;     `(,a ,b ,c))))


; (let ((a 'a) (b 'b))
;   `(,a ,b ,(let ((c 'c) (d 'd) (e 'e))
;     `(,a ,b ,c ,d ,e))))




; ; ;works
; (let ((a 'a) (b 'b))
;   `(,a ,b ,(let ((c 'c) (d 'd))
;     `(,a ,b ,c,d  ,(let
;         ((e 'e)) #t)))))


; ; ;works
; (let ((a 'a) (b 'b))
;   `(,a ,b ,(let ((c 'c) (d 'd) (x 'x))
;     `(,a ,b ,c ,x ,(let
;         ((e 'e)) #t)))))


; ; ;works
; (let ((a 'a) (b 'b) (x 'x))
;   `(,a ,b ,(let ((c 'c) (d 'd))
;     `(,a ,b ,c ,d ,x))))



; ;not works
; (let ((a 'a) (b 'b))
;   `(,(let ((c 'c))
;     `(,a ,b ,c ,(let ((d 'd)) `(,a ,b ,c ,d))))))


; (map + '(1 2) '(3 4))


; (+ 1 2 3 4 5 )
; (define x (+ 2))
; ((lambda (x)
; 	(set! x (+ 2 3))
; 	x) x)

; (define x (+ 2))

; ((lambda (x)
; 	(set! x (+ 2 3))
; 	x) x)

; (define x (+ 2))

; ((lambda (x)
; 	(set! x (+ 2 3))
; 	x) x)

; (define str (make-string 5 #\space))

; (string-set! str 0 #\t)
; (string-set! str 1 #\l)
; (string-set! str 3 #\n)
; (string-set! str 4 #\newline)

; str
; ((lambda x
;      (map cdr x))
;    '(97 #\a) (list 0 #\nul) `(32.0 #\space) (cons 10 #\newline))

(equal? (let ((ascii
	(lambda ()
		(letrec ((loop (lambda (i)
						(if (< i 127)
							(cons `(,i ,(integer->char i)) (loop (+ 1 i)))
							'()))))
			(loop (char->integer #\space))))))

(ascii) ) '((32 #\space) (33 #\!) (34 #\") (35 #\#) (36 #\$) (37 #\%)
 (38 #\&) (39 #\') (40 #\() (41 #\)) (42 #\*) (43 #\+)
 (44 #\,) (45 #\-) (46 #\.) (47 #\/) (48 #\0) (49 #\1)
 (50 #\2) (51 #\3) (52 #\4) (53 #\5) (54 #\6) (55 #\7)
 (56 #\8) (57 #\9) (58 #\:) (59 #\;) (60 #\<) (61 #\=)
 (62 #\>) (63 #\?) (64 #\@) (65 #\A) (66 #\B) (67 #\C)
 (68 #\D) (69 #\E) (70 #\F) (71 #\G) (72 #\H) (73 #\I)
 (74 #\J) (75 #\K) (76 #\L) (77 #\M) (78 #\N) (79 #\O)
 (80 #\P) (81 #\Q) (82 #\R) (83 #\S) (84 #\T) (85 #\U)
 (86 #\V) (87 #\W) (88 #\X) (89 #\Y) (90 #\Z) (91 #\[)
 (92 #\\) (93 #\]) (94 #\^) (95 #\_) (96 #\`) (97 #\a)
 (98 #\b) (99 #\c) (100 #\d) (101 #\e) (102 #\f) (103 #\g)
 (104 #\h) (105 #\i) (106 #\j) (107 #\k) (108 #\l) (109 #\m)
 (110 #\n) (111 #\o) (112 #\p) (113 #\q) (114 #\r) (115 #\s)
 (116 #\t) (117 #\u) (118 #\v) (119 #\w) (120 #\x) (121 #\y)
 (122 #\z) (123 #\{) (124 #\|) (125 #\}) (126 #\~)))



 (equal? (letrec ( (list? (lambda (x)
	(or (null? x) (and (pair? x) (list? (cdr x)))))) (describe
  (lambda (e)
    (cond
     ((list? e) `(list ,@(map describe e)))
     ((pair? e) `(cons ,(describe (car e))
        ,(describe (cdr e))))
     ((or (null? e) (symbol? e)) `',e)
     (else e)))) ) (describe '(sym "str" #\c 1))) '(list 'sym "str" #\c 1))

;  (define list? (lambda (x)
; 	(or (null? x) (and (pair? x) (list? (cdr x))))))

; (define describe
;   (lambda (e)
;     (cond
;      ((list? e) `(list ,@(map describe e)))
;      ((pair? e) `(cons ,(describe (car e))
;         ,(describe (cdr e))))
;      ((or (null? e) (symbol? e)) `',e)
;      (else e))))

; (describe '(sym "str" #\c 1))
