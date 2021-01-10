;less args in TP
((lambda (x y) ((lambda (x)  x) x)) 2 3)

((lambda ()
    ((lambda (a b c d e) e)
        'a 'b 'c 'd 'e)))

((lambda (x y) ((lambda (x y) ((lambda (z) z) y)) x y)) 2 3)

((lambda (x y) ((lambda (x y) y) x y)) 2 3)