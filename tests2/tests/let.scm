(let ((x 3) (y 4)) (+ x y))

(letrec ((x 5) (y 4)) (+ x y))

(let* ((x 3) (y (+ x 1))) y)

  ; ;works
(let ((a 'a) (b 'b) (x 'x)) 
  `(,a ,b ,(let ((c 'c) (d 'd)) 
    `(,a ,b ,c ,d ,x))))

;not works
(let ((a 'a) (b 'b)) 
  `(,(let ((c 'c)) 
    `(,a ,b ,c ,(let ((d 'd)) `(,a ,b ,c ,d))))))

;works
(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c)) 
    `(,a ,b ,c))))


(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c) (d 'd) (e 'e)) 
    `(,a ,b ,c ,d ,e))))




; ;works
(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c) (d 'd)) 
    `(,a ,b ,c,d  ,(let
        ((e 'e)) #t)))))


; ;works
(let ((a 'a) (b 'b)) 
  `(,a ,b ,(let ((c 'c) (d 'd) (x 'x)) 
    `(,a ,b ,c ,x ,(let
        ((e 'e)) #t)))))

 (let ((a 'a) (b 'b)) `(,a ,b ,(let ((c 'c)) `(,a ,b ,c ,(let
((d 'd) (e 'e) (f 'f)) `(,a ,b ,c ,d ,e ,f ,(let () `(,a ,b ,c ,d ,e
,f ,(let ((g 'g) (h 'h)) `(,a ,b ,c ,d ,e ,f ,g ,h ,(let ((i 'i) (j
'j) (k 'k) (l 'l)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let () `(,a
,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,(let ((m 'm) (n 'n)) `(,a ,b ,c ,d
,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,(let ((o 'o) (p 'p) (q 'q) (r 'r)) `(,a
,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c
,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let () `(,a ,b ,c ,d ,e
,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,(let ((s 's) (t 't)) `(,a ,b
,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,(let ((u 'u) (v
'v) (w 'w)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s
,t ,u ,v ,w ,(let () `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p
,q ,r ,s ,t ,u ,v ,w ,(let ((x 'x)) `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k
,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,(let ((y 'y) (z 'z)) `(,a ,b
,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u ,v ,w ,x ,y
,z))))))))))))))))))))))))))))))))

(let ((a 'a))
  (let ((b 'b) (c 'c))
    (let ()
      (let ()
        (let ((d 'd) (e 'e) (f 'f) (g 'g))
          (let ()
            (let ((h 'h) (i 'i))
              (let ((j 'j) (k 'k) (l 'l) (m 'm) (n 'n))
                (let ()
                  (let ((o 'o))
                    (let ((p 'p) (q 'q))
                      `(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p))))))))))))



(let ((a 'a))
  `(ha ,(let ((b 'b) (c 'c))
           `(ha ,(let ()
                    `(ha ,(let ()
                             `(ha ,(let ((d 'd) (e 'e) (f 'f) (g 'g))
                                      `(ha ,(let ()
                                               `(ha ,(let ((h 'h) (i 'i))
                                                        `(ha ,(let ((j 'j) (k 'k) (l 'l) (m 'm) (n 'n))
                                                                 `(ha ,(let ()
                                                                          `(ha ,(let ((o 'o))
                                                                                   `(ha ,(let ((p 'p) (q 'q))
                                                                                            `(ha ,`(,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p)))))))))))))))))))))))