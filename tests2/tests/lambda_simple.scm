;(define - (lambda (x y) (+ x ( * y -1))))
(define id (lambda (x) x))
(define add (lambda (x y) (+ x y)))
(define fact (lambda (n) (if (= n 0) 1 (* n (fact (+ n -1))))))
(define nested (lambda (x) (lambda (y) (lambda (z) (* z x)))))
((lambda (x) x) 5)
(id 4)
(add 3 4)
;(fact 5)
(((nested 3) 4 ) 5)
(((((lambda () (lambda () (lambda () (lambda () 5))))))))

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

((((lambda (x) (x x))
   (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
  (lambda (x) (+ x 1)))
 0)


(((((lambda (x) (x x))
   (lambda (x) (lambda (y) (x (x (x (x (x y))))))))
  (lambda (p) (p (lambda (x y) (lambda (p) (p y x))))))
  (lambda (x) (x 'moshe 'yossi)))
 (lambda (x y) (cons x (cons y '()))))

;((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
;
;
;
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

(define fact
  ((lambda (x) (x x))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((fact fact) (- n 1))))))))


(fact 5)

;((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s 
;((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s 
;((lambda s ((lambda s s) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) 'mary 'had 'a 'little 'lambda)