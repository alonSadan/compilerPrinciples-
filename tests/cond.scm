(cond
    (#t 2) 
    (#f 3))

(cond
    (#f 2) 
    (#t 3))

(cond
    (#f 2) 
    (#f 3)
    (else 5))

(cond
      ('(2 3) => pair?) 
      (#f 3)
      (else 5))
