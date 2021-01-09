; no opt
((lambda (x . y) y) 1)

; have opt
((lambda (a b c . d) d) 1 1 2 3 5 8)

; lambda variadic no args
((lambda x x))

; lambda variadic with args
((lambda x x) 1 2 3)