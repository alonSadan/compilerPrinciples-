; ; (define map-one 
; ;   (lambda (f s) 
; ;     (if (null? s) '() 
; ;       (cons 
; ;         (f (car s))
; ;         (map-one f (cdr s))))))


; ; (define map-many
; ; 	(lambda (f lists)
; ; 	  (if (null? (car lists))
; ; 		  '()
; ; 		  (cons
; ; 		   (apply f (map-one car lists))
; ; 		   (map-many f (map-one cdr lists))))))

; ; (define map (lambda (f . args) (map-many f args)))

; ; (map + '(1 2) '(3 4))

; ; ((lambda (x y) ((lambda (x)  x) x)) 2 3)

; ; ((lambda ()
; ;     ((lambda (a b c d e) e)
; ;         'a 'b 'c 'd 'e)))

; ; (((((lambda (a) (lambda (b) (((lambda (a) (lambda (b) ((a b) (lambda
; ; (x) (lambda (y) y))))) ((lambda (n) ((n (lambda (x) (lambda (x)
; ; (lambda (y) y)))) (lambda (x) (lambda (y) x)))) (((lambda (a) (lambda
; ; (b) ((b (lambda (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n
; ; (lambda (p) (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
; ; ((lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p
; ; (lambda (a) (lambda (b) a)))) p))) ((lambda (p) (p (lambda (a) (lambda
; ; (b) a)))) p)))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
; ; (lambda (x) (lambda (y) y))) (lambda (x) (lambda (y) y))))))) a))) a)
; ; b))) ((lambda (n) ((n (lambda (x) (lambda (x) (lambda (y) y))))
; ; (lambda (x) (lambda (y) x)))) (((lambda (a) (lambda (b) ((b (lambda
; ; (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n (lambda (p)
; ; (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) ((lambda (n)
; ; (lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p (lambda (a)
; ; (lambda (b) a)))) p))) ((lambda (p) (p (lambda (a) (lambda (b) a))))
; ; p)))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) (lambda (x)
; ; (lambda (y) y))) (lambda (x) (lambda (y) y))))))) a))) b) a)))))
; ; ((lambda (n) ((lambda (p) (p (lambda (a) (lambda (b) b)))) ((n (lambda
; ; (p) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) ((lambda (n)
; ; (lambda (s) (lambda (z) (s ((n s) z))))) ((lambda (p) (p (lambda (a)
; ; (lambda (b) a)))) p))) (((lambda (a) (lambda (b) ((b (a (lambda (a)
; ; (lambda (b) ((a (lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))))
; ; b))))) (lambda (x) (lambda (y) y))))) ((lambda (p) (p (lambda (a)
; ; (lambda (b) a)))) p)) ((lambda (p) (p (lambda (a) (lambda (b) b))))
; ; p))))) (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) (lambda (x)
; ; x)) (lambda (x) x))))) (lambda (x) (lambda (y) (x (x (x (x (x
; ; y))))))))) (((lambda (a) (lambda (b) ((b (a (lambda (a) (lambda (b)
; ; ((a (lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))) b)))))
; ; (lambda (x) (lambda (y) y))))) (((lambda (a) (lambda (b) ((b (a
; ; (lambda (a) (lambda (b) ((a (lambda (n) (lambda (s) (lambda (z) (s ((n
; ; s) z)))))) b))))) (lambda (x) (lambda (y) y))))) ((lambda (x) (lambda
; ; (y) (x (x (x y))))) (lambda (x) (lambda (y) (x (x y)))))) (lambda (x)
; ; (lambda (y) (x (x (x y))))))) (lambda (x) (lambda (y) (x (x (x (x (x
; ; y))))))))) #t) #f)


; ; (define fold-right
; ; 	(let ((null? null?)
; ; 		(car car) (cdr cdr))
; ;     (lambda (f acc lst)
; ;       (if (null? lst) acc
; ;         (f (car lst) (fold-right f acc (cdr lst)))))))



; ; (define zero?
; ;   (let ((= =))
; ;     (lambda (x) (= x 0))))


; ; (define ^f
; ;   (lambda (f)
; ;     (lambda (x y)
; ;       `(,f ,x ,y))))

; ; (define f (^f 'f))
; ; (define g (^f 'g))

; ; (fold-right g 'u '(x1 x2 x3 x4 x5))

; ; (define map
; ;   (let ((null? null?)
; ; 	(car car) (cdr cdr)
; ; 	(cons cons) (apply apply))
; ;   (letrec ((map-many
; ; 	    (lambda (f lists)
; ; 	      (if (null? (car lists))
; ; 		  '()
; ; 		  (cons
; ; 		   (apply f (map-one car lists))
; ; 		   (map-many f (map-one cdr lists))))))
; ; 	   (map-one
; ; 	    (lambda (f s)
; ; 	      (if (null? s)
; ; 		  '()
; ; 		  (cons (f (car s))
; ; 			(map-one f (cdr s)))))))
; ;     (lambda (f . args)
; ;       (map-one f args)))))


; ; (define map 
; ;   (lambda (f s)
; ; 	    (if (null? s)
; ; 		  '()
; ; 		  (cons (f (car s))))))


; ; (map map
; ;   `(,map)
; ;   `((,map))
; ;   `(((,map)))
; ;   `((((,map))))
; ;   `(((((,map)))))
; ;   `((((((,map))))))
; ;   `(((((((,map)))))))
; ;   `((((((((,map))))))))
; ;   `(((((((((,list)))))))))
; ;   `((((((((((a (b c) d)))))))))))




; ; (define map
; ;   (lambda (f s)
; ;     (if (null? s)
; ;         '()
; ;         (cons (f (car s))
; ;           (map f (cdr s))))))

; ; (map list '(4 9 6 3 5 1))

; ; (define mapcar
; ;   (let* ((y (lambda (f)
; ;               ((lambda (x) (x x))
; ;                (lambda (x)
; ;                  (f (lambda s (apply (x x) s)))))))
; ;          (map1
; ;            (y (lambda (map1)
; ;                 (lambda (f s)
; ;                   (if (null? s)
; ;                       '()
; ;                       (cons (f (car s))
; ;                         (map1 f (cdr s))))))))
; ;          (map-list
; ;            (y (lambda (map-list)
; ;                 (lambda (f s)
; ;                   (if (null? (car s))
; ;                       '()
; ;                       (cons (apply f (map1 car s))
; ;                         (map-list f (map1 cdr s)))))))))
; ;     (lambda (f . s)
; ;       (map-list f s))))

; ; (mapcar + '(1 2) '(3 4))


; ; `(ha ,(apply cons 1 '(2)))

; ; (apply cons 1 3 '(2))

; ; (define append
; ;   (let ((null? null?)
; ; 	(fold-right fold-right)
; ; 	(cons cons))
; ;     (lambda args
; ;       (fold-right
; ;        (lambda (e a)
; ; 	 (if (null? a)
; ; 	     e
; ; 	     (fold-right cons a e)))
; ;        '() args))))

; ; (append '(1 2) 3)
; ; (define s '(1))
; ; `(,@s '())


((((lambda (x) (x x))
   (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
  (lambda (x) (+ x 1)))
 0)


(((((lambda (x) (x x))
   (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
  (lambda (p) (p (lambda (x y) (lambda (p) (p y x))))))
  (lambda (x) (x 'moshe 'yossi)))
 (lambda (x y) (cons x (cons y '()))))

((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))



((((((lambda (x) (x x))
     (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
    (lambda (f)
      (lambda (x)
        (x (f x)))))
   ((lambda (x) (x x))
    (lambda (x) (lambda (f) (f (lambda (y) ((x x) y)))))))
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1)))))))
 5)

((((((lambda (x) (x x))
     (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
    (lambda (f)
      (lambda (x)
        (x (f x)))))
   (lambda (f)
     ((lambda (x) (x x))
      (lambda (x) (f (lambda (y) ((x x) y)))))))
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1)))))))
 5)

(((lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y))))))
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1)))))))
 5)

