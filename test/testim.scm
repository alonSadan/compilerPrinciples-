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

; (define map-one
;   (lambda (f s)
;     (if (null? s) '()
;       (cons
;         (f (car s))
;         (map-one f (cdr s))))))


; (define map-many
; 	(lambda (f lists)
; 	  (if (null? (car lists))
; 		  '()
; 		  (cons
; 		   (apply f (map-one car lists))
; 		   (map-many f (map-one cdr lists))))))

; (define map (lambda (f . args) (map-many f args)))

; (map + '(1 2) '(3 4))

; ((lambda (x y) ((lambda (x)  x) x)) 2 3)

; ((lambda ()
;     ((lambda (a b c d e) e)
;         'a 'b 'c 'd 'e)))



(((((lambda (a) (lambda (b) (((lambda (a) (lambda (b) ((a b) (lambda
(x) (lambda (y) y))))) ((lambda (n) ((n (lambda (x) (lambda (x)
(lambda (y) y)))) (lambda (x) (lambda (y) x)))) (((lambda (a) (lambda
(b) ((b (lambda (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n
(lambda (p) (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
((lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p
(lambda (a) (lambda (b) a)))) p))) ((lambda (p) (p (lambda (a) (lambda
(b) a)))) p)))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
(lambda (x) (lambda (y) y))) (lambda (x) (lambda (y) y))))))) a))) a)
b))) ((lambda (n) ((n (lambda (x) (lambda (x) (lambda (y) y))))
(lambda (x) (lambda (y) x)))) (((lambda (a) (lambda (b) ((b (lambda
(n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n (lambda (p)
(((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) ((lambda (n)
(lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p (lambda (a)
(lambda (b) a)))) p))) ((lambda (p) (p (lambda (a) (lambda (b) a))))
p)))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) (lambda (x)
(lambda (y) y))) (lambda (x) (lambda (y) y))))))) a))) b) a)))))
((lambda (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n (lambda
(p) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) ((lambda (n)
(lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p (lambda (a)
(lambda (b) a)))) p))) (((lambda (a) (lambda (b) ((b (a (lambda (a)
(lambda (b) ((a (lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))))
b))))) (lambda (x) (lambda (y) y))))) ((lambda (p) (p (lambda (a)
(lambda (b) a)))) p)) ((lambda (p) (p (lambda (a) (lambda (b) b))))
p))))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) (lambda (x)
x)) (lambda (x) x))))) (lambda (x) (lambda (y) (x (x (x (x (x
y))))))))) (((lambda (a) (lambda (b) ((b (a (lambda (a) (lambda (b)
((a (lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))) b)))))
(lambda (x) (lambda (y) y))))) (((lambda (a) (lambda (b) ((b (a
(lambda (a) (lambda (b) ((a (lambda (n) (lambda (s) (lambda (z) (s ((n
s) z)))))) b))))) (lambda (x) (lambda (y) y))))) ((lambda (x) (lambda
(y) (x (x (x y))))) (lambda (x) (lambda (y) (x (x y)))))) (lambda (x)
(lambda (y) (x (x (x y))))))) (lambda (x) (lambda (y) (x (x (x (x (x
y))))))))) #t) #f)


; (let ()
; ((lambda s
; (let ()
; ((lambda s s) s s s)))
; #t))

; ((((lambda (x) ((x x) x))
; (lambda (x) (lambda (y) (x (x y)))))
; (lambda (x) (+ x 1)))
; 0)





;  (define minus_fold (lambda nums
;                        (fold-right (lambda (num delta)
;                                      (+ delta (* -1 num)))
;                          (car nums)
;                          (cdr nums))))

; ; (minus_fold 1 2 3 4)

; (apply apply `(,apply (,apply (,+ (1 2 3)))))
; (apply
; apply
; `(,apply
; (,apply
; (,apply
; (,apply
; (,apply
; (,apply
; (,apply (,apply (,apply (,apply (,+ (1 2 3)))))))))))))


; (define fold-right
; 	(let ((null? null?)
; 		(car car) (cdr cdr))
;     (lambda (f acc lst)
;       (if (null? lst) acc
;         (f (car lst) (fold-right f acc (cdr lst)))))))


;  (define minus_fold (lambda nums
;                        (fold-right (lambda (num delta)
;                                      (+ delta (* 1 num)))
;                          (car nums)
;                          (cdr nums))))

; (minus_fold 1 2 3 4)
; (define x (lambda (a . b) (+ a (car (cdr b)) ) ))
; (x 1 4 3 4 5)

; (apply
;   apply
;   `(,apply
;      (,apply
;        (,apply
;          (,apply
;            (,apply
;              (,apply
;                (,apply
;                  (,apply (,apply (,apply (,(lambda s s) (alon)))))))))))))


; (define fold-left
; 	(let ((null? null?)
; 		(car car) (cdr cdr))
;     (lambda (f acc lst)
;       (if (null? lst) acc
;       	(fold-left f (f acc (car lst)) (cdr lst))))))

; (define ^f
;   (lambda (f)
;     (lambda (x y)
;       `(,f ,x ,y))))

; (define f (^f 'f))
; (define g (^f 'g))

; (fold-right g 'u '(x1 x2 x3 x4 x5))


; (fold-left g 'u '(x1 x2 x3 x4 x5))


; (define list (lambda x x))

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

; ((((lambda (x) ((x x) x))
;    (lambda (x) (lambda (y) (x (x y)))))
;   (lambda (x) (+ x 1)))
;  0)


; ((((lambda (x) ((x x) x))
; (lambda (x) (lambda (y) (x (x y)))))
; (lambda (x) (+ x 1)))
; 0)


; ( define map
; 	    (lambda (f s)
; 	      (if (null? s)
; 		  '()
; 		  (cons (f (car s))
; 			(map f (cdr s))))))

; (define map
;   (lambda (f s)
;     (if (null? s)
;         '()
;         (cons (f (car s))
;           (map f (cdr s))))))

; (map list '(4 9 6 3 5 1))

; (apply (lambda (x y z w) `((x ,x) (y ,y) (z ,z) (w ,w)))
;   '(4 9 6 3))

; (apply (lambda (x y z w . rest) `((x ,x) (y ,y) (z ,z) (w ,w) (rest ,rest)))
;   '(4 9 6 3 5 1))

; (map map
;   `(,map)
;   `((,map))
;   `(((,map)))
;   `((((,map))))
;   `(((((,map)))))
;   `((((((,map))))))
;   `(((((((,map)))))))
;   `((((((((,map))))))))
;   `(((((((((,list)))))))))
;   `((((((((((a (b c) d)))))))))))


; ((lambda (a b . c) `((a ,a) (b ,b) (c ,c))) 4 9 6 3 5 1)
; ((a 4) (b 9) (c (6 3 5 1)))

; (define map
;   (letrec ((map1
;              (lambda (f s)
;                (if (null? s)
;                    '()
;                    (cons (f (car s)
;                            (map1 f (cdr s)))))))
;            (map-list
;              (lambda (f s)
;                (if (null? (car s))
;                    '()
;                    (cons (apply f (map1 car s))
;                      (map-list f (map1 cdr s)))))))
;     (lambda (f . s)
;       (map-list f s))))

; (map list '(1 2 3))



; (define ^f
;   (lambda (f)
;     (lambda (x y)
;       `(,f ,x ,y))))

; (define map
;   (let* ((y (lambda (f)
;               ((lambda (x) (x x))
;                (lambda (x)
;                  (f (lambda s (apply (x x) s)))))))
;          (map1
;            (y (lambda (map1)
;                 (lambda (f s)
;                   (if (null? s)
;                       '()
;                       (cons (f (car s))
;                         (map1 f (cdr s))))))))
;          (map-list
;            (y (lambda (map-list)
;                 (lambda (f s)
;                   (if (null? (car s))
;                       '()
;                       (cons (apply f (map1 car s))
;                         (map-list f (map1 cdr s)))))))))
;     (lambda (f . s)
;       (map-list f s))))


; (map + '(1 2 3) '(4 5 6))
; (define fold-right
; 	(let ((null? null?)
; 		(car car) (cdr cdr))
;     (lambda (f acc lst)
;       (if (null? lst) acc
;         (f (car lst) (fold-right f acc (cdr lst)))))))



; (define -
;   (lambda (a b)
;     (+ a (* -1 b))))


; (define zero?
;   (let ((= =))
;     (lambda (x) (= x 0))))

; (define append
;   (let ((null? null?)
; 	(fold-right fold-right)
; 	(cons cons))
;     (lambda args
;       (fold-right
;        (lambda (e a)
; 	 (if (null? a)
; 	     e
; 	     (fold-right cons a e)))
;        '() args))))

; (append '(1) '(2))

; (define my-append
;   (letrec ((binary-append
;              (lambda (s1 s2)
;                (if (null? s1)
;                    s2
;                    (cons (car s1)
;                      (binary-append (cdr s1) s2))))))
;     (lambda args
;       (fold-right binary-append '() args))))



; (define binary-append
;   (lambda (s1 s2)
;     (if (null? s1)
;         s2
;         (cons (car s1)
;           (binary-append (cdr s1) s2)))))

; (define my-append
;   (lambda args
;     (fold-right binary-append '() args)))


; (define my-append
;   (let* ((y (lambda (f)
;               ((lambda (x) (x x))
;                (lambda (x)
;                  (f (lambda s (apply (x x) s)))))))
;          (binary-append
;            (y (lambda (binary-append)
;                 (lambda (s1 s2)
;                   (if (null? s1)
;                       s2
;                       (cons (car s1)
;                         (binary-append (cdr s1) s2))))))))
;     (lambda args
;       (fold-right binary-append '() args))))

; (define my-append
;   (let* ((y (lambda (f)
;               ((lambda (x) (x x))
;                (lambda (x)
;                  (f (lambda (arg1 arg2) ((x x) arg1 arg2)))))))
;          (binary-append
;            (y (lambda (binary-append)
;                 (lambda (s1 s2)
;                   (if (null? s1)
;                       s2
;                       (cons (car s1)
;                         (binary-append (cdr s1) s2))))))))
;     (lambda args
;       (fold-right binary-append '() args))))

; (define my-append
;   (let* ((y (lambda (f)
;               ((lambda (x) (x x))
;                (lambda (x)
;                  (f (lambda (arg1 arg2) ((x x) arg1 arg2)))))))
;          (binary-append
;            (y (lambda (binary-append)
;                 (lambda (s1 s2)
;                   (if (null? s1)
;                       s2
;                       (cons (car s1)
;                         (binary-append (cdr s1) s2))))))))
;     (lambda (args)
;       (fold-right binary-append '() args))))

; (define y
;   (lambda (f)
;     ((lambda (x) (x x))
;      (lambda (x)
;        (f (lambda (arg1 arg2) ((x x) arg1 arg2)))))))

; (define binary-append
;   (y (lambda (binary-append)
;        (lambda (s1 s2)
;          (if (null? s1)
;              s2
;              (cons (car s1)
;                (binary-append (cdr s1) s2)))))))

; (define append-list
;   (lambda (args)
;     (fold-right binary-append '() args)))


; (append-list '((a b c) (d e) (1 2 3)))


; (define binary-append
;   (lambda (s1 s2)
;     (if (null? s1)
;         s2
;         (cons (car s1)
;           (binary-append (cdr s1) s2)))))

; (define append-list
;   (lambda (args)
;     (fold-right binary-append '() args)))

; (define fix2
;   (lambda (f)
;     (f (lambda (a1 a2)
;          ((fix2 f) a1 a2)))))

; (define binary-append
;   (fix2 (lambda (binary-append)
;           (lambda (s1 s2)
;             (if (null? s1)
;                 s2
;                 (cons (car s1)
;                   (binary-append (cdr s1) s2)))))))

; (define append-list
;   (lambda (args)
;     (fold-right binary-append '() args)))

; (append-list '((a b c) (d e) (1 2 3)))

; (define fact
;   ((lambda (x) (x x))
;    (lambda (fact)
;      (lambda (n)
;        (if (zero? n)
;            1
;            (* n ((fact fact) (- n 1))))))))

; (define fact
;   (((lambda (x) (x x))
;     (lambda (x)
;       (lambda (f)
;         (f (lambda (a)
;              (((x x) f) a))))))
;    (lambda (fact)
;      (lambda (n)
;        (if (zero? n)
;            1
;            (* n (fact (- n 1))))))))


; (define fact
;   ((lambda (f)
;      ((lambda (x) (x x))
;       (lambda (x)
;         (f (lambda (a)
;              ((x x) a))))))
;    (lambda (fact)
;      (lambda (n)
;        (if (zero? n)
;            1
;            (* n (fact (- n 1))))))))

; (fact 5)
; (my-append '(1) '(2) '(3))






; (define funny
;   (lambda (n)
;     (if (zero? n)
;         '()
;         (apply cons `(ha ,(funny (- n 1)))))))

;  (funny 10)
; (define foo
;   (lambda (n . s)
;     (if (zero? n)
;         s
;         (apply foo (- n 1) `(ha ,@s)))))

; (foo 10)

; (define funny
;   (lambda (n)
;     (if (zero? n)
;         '()
;         `(ha ,@(apply cons `(ha ,(funny (- n 1))))))))
;         (funny 10)

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

; ((((lambda (x) (x x))
;    (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
;   (lambda (x) (+ x 1)))
;  0)

; (((((lambda (x) (x x))
;    (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
;   (lambda (p) (p (lambda (x y) (lambda (p) (p y x))))))
;   (lambda (x) (x 'moshe 'yossi)))
;  (lambda (x y) (cons x (cons y '()))))
; ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
; ((lambda (x) (quasiquote ((unquote x) (quote (unquote x))))) (quote (lambda (x) (quasiquote ((unquote x) (quote (unquote x)))))))

; ((((((lambda (x) (x x))
;      (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
;     (lambda (f)
;       (lambda (x)
;         (x (f x)))))
;    ((lambda (x) (x x))
;     (lambda (x) (lambda (f) (f (lambda (y) ((x x) y)))))))
;   (lambda (fact)
;     (lambda (n)
;       (if (zero? n)
;           1
;           (* n (fact (- n 1)))))))
;  5)

; ((((((lambda (x) (x x))
;      (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
;     (lambda (f)
;       (lambda (x)
;         (x (f x)))))
;    (lambda (f)
;      ((lambda (x) (x x))
;       (lambda (x) (f (lambda (y) ((x x) y)))))))
;   (lambda (fact)
;     (lambda (n)
;       (if (zero? n)
;           1
;           (* n (fact (- n 1)))))))
;  5)

; (((lambda (f)
;     ((lambda (x) (x x))
;      (lambda (x) (f (lambda (y) ((x x) y))))))
;   (lambda (fact)
;     (lambda (n)
;       (if (zero? n)
;           1
;           (* n (fact (- n 1)))))))
;  5)

; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
; ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j 'j) (k 'k)
; (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d ,e ,f ,g
; ,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f
; ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h
; ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let ((s 's) (t 't)) `(,a ,b ,c ,d ,e
; ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,(let ((u 'u) (v 'v) (w
; 'w)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u
; ,v ,w ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r
; ,s ,t ,u ,v ,w ,(let ((x 'x)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m
; ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,(let ((y 'y) (z 'z)) `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,y
; ,z))))))))))))))))))))))))))))))))

; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
; ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j 'j) (k 'k)
; (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d ,e ,f ,g
; ,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f
; ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h
; ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let ((s 's) (t 't)) `(,a ,b ,c ,d ,e
; ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,(let ((u 'u) (v 'v) (w
; 'w)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u
; ,v ,w ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r
; ,s ,t ,u ,v ,w ,(let ((x 'x)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m
; ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ))))))))))))))))))))))))))))))


; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
; ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j 'j) (k 'k)
; (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d ,e ,f ,g
; ,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f
; ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h
; ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let ((s 's) (t 't)) `(,a ,b ,c ,d ,e
; ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,(let ((u 'u) (v 'v) (w
; 'w)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u
; ,v ,w ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r
; ,s ,t ,u ,v ,w ))))))))))))))))))))))))))))


; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
; ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j 'j) (k 'k)
; (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d ,e ,f ,g
; ,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ))))))))))))))))))


; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
; ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j 'j) (k 'k)
; (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a ,b ,c ,d
; ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d ,e ,f ,g
; ,h ,i ,j ,k ,l ,m ,n ))))))))))))))))


; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
; ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j 'j) (k 'k)
; (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l))))))))))))



; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ,(let
; ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ))))))))))


; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e ,f ))))))))

; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c))))

(let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
) `(,a ,b ,c ,d ))))))

; val ast : expr' list =                                                                                                                                       [Applic'                                                                                                                                                   (LambdaSimple' (["a"; "b"],
;       Applic' (Var' (VarFree "cons"),
;        [Var' (VarParam ("a", 0));
;         Applic' (Var' (VarFree "cons"),
;          [Var' (VarParam ("b", 1));
;           Applic' (Var' (VarFree "cons"),
;            [Applic'
;              (LambdaSimple' (["c"],
;                Applic' (Var' (VarFree "cons"),
;                 [Var' (VarBound ("a", 0, 0));
;                  Applic' (Var' (VarFree "cons"),
;                   [Var' (VarBound ("b", 0, 1));
;                    Applic' (Var' (VarFree "cons"),
;                     [Var' (VarParam ("c", 0));
;                      Applic' (Var' (VarFree "cons"),
;                       [Applic'
;                         (LambdaSimple' (["d"],
;                           Applic' (Var' (VarFree "cons"),
;                            [Var' (VarBound ("a", 1, 0));
;                             Applic' (Var' (VarFree "cons"),
;                              [Var' (VarBound ("b", 1, 1));
;                               Applic' (Var' (VarFree "cons"),
;                                [Var' (VarBound ("c", 0, 0));
;                                 Applic' (Var' (VarFree "cons"),
;                                  [Var' (VarParam ("d", 0)); Const' (Sexpr Nil)])])])])),
;                         [Const' (Sexpr (Symbol "d"))]);
;                        Const' (Sexpr Nil)])])])])),
;              [Const' (Sexpr (Symbol "c"))]);
;             Const' (Sexpr Nil)])])])),
;     [Const' (Sexpr (Symbol "a")); Const' (Sexpr (Symbol "b"))])]


; (let ((a 1) (c 3)) (let ((b 2)) (let ((f 6) ) (+ a (+ b (+ c (+ 1 (+ 1 (+ f 1))))))) ))

; val ast : expr' list =                                                                                                                                     [Applic' (LambdaSimple' (["a"], Var' (VarParam ("a", 0))),
; [Const' (Sexpr (Number (Fraction (1, 1))))])]
; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let ((d 'd)
; (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ))))))


; (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ))))
