(define x 0)
(set! x 3)
(if #f 1 x)
(if #t (set! x 2) x)
x