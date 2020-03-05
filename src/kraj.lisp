;; POMOCNA
;; proverava da li je vrsta prazna
(defun proveriPraznuVrstu (vrsta &optional rez)
    (if (not vrsta)
        rez
        (if (or (equal (car vrsta) 'BELO) (not (car vrsta)))
            (proveriPraznuVrstu (cdr vrsta) T)
            (proveriPraznuVrstu NIL NIL)
        )
    )
)

;; POMOCNA
;; proverava da li je tabla prazna
(defun proveriPraznuTablu (tabla &optional rez)
    (if (not tabla)
        rez
        (if (proveriPraznuVrstu (car tabla))
            (proveriPraznuTablu (cdr tabla) T)
            (proveriPraznuTablu NIL NIL)
        )
    )
)

;; GLAVNA
;; proverava da li je prosledjeno stanje zavrsena igra i vraca
;; NIL - ako nije kraj
;; X ili O - pobednik
(defun proveriKraj (stanje)
    (if (not (proveriPraznuTablu (cdr stanje)))
        NIL
        (if (> (caadar stanje) (car (cdadar stanje)))
            'X
            'O
        )
    )
)