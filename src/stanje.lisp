;; POMOCNA
;; vraca listu (a b a b a b ...) do n
(defun startPolje (a b n)
    (cond 
        ((= n 0) '())
        ((= (mod n 2) 0) (cons a (startPolje a b (- n 1))))
        ((= (mod n 2) 1) (cons b (startPolje a b (- n 1))))
    )

)

;; POMOCNA
;; vraca delimicnu pocetnu tablu
(defun startTabla (n n1)
    (cond 
        ((= n 0) '())
        ((= (mod n 2) 0) 
            (cons 
                (startPolje 'BELO '(X) n1)
                (startTabla (- n 1) n1)
            )
        )
        ((= (mod n 2) 1) 
            (cons 
                (startPolje '(O) 'BELO n1)
                (startTabla (- n 1) n1)
            )
        )
    )
)

;; GLAVNA
;; vraca pocetno stanje igre za tablu velicine n*n
(defun byteStart (n)
    (cons 
        (list 'X '(0 0))
        (append
            (list (startPolje NIL 'BELO n))
            (startTabla (- n 2) n)
            (list (startPolje 'BELO NIL n))
        )
    )
)

;; (print byteStart 8)