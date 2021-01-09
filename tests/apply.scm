(apply + '(1 2))

(apply (lambda (x y z w) y) 1 2 '(3 4))

(define funny
    (lambda (n)
    (if (zero? n)
        '()
        `(ha ,(apply cons `(ha ,(funny (- n 1))))))))
(funny 2)

 (define foo
  (lambda (n . s)
    (if (zero? n)
        s
        (apply foo (- n 1) `(ha ,s)))))

(foo 3) 