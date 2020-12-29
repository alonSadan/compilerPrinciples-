(begin (define x 5) x)
(set! x 3)
(if #f 1 x)
(if #t (set! x 2) x)
x