;; POMOCNE
;; naredne tri funkcije sluze da izvucu koordinate svih plocica datog igraca (vrsta, kolona, visiina)
(defun izdvojiSveKoordinate (polje x y v igrac)
    (if (not polje)
        NIL
        (if (not (equal (car polje) igrac))
            (izdvojiSveKoordinate (cdr polje) x y (+ 1 v) igrac)
            (cons (list x y v) (izdvojiSveKoordinate (cdr polje) x y (+ v 1) igrac))
        )
    )
)


(defun izdvojiSvaPolja (vrsta x y v igrac)
    (if (not vrsta)
        NIL
        (if (or (equal (car vrsta) 'BELO) (not (member igrac (car vrsta))))
            (izdvojiSvaPolja (cdr vrsta) x (+ 1 y) v igrac)
            (append (izdvojiSveKoordinate (car vrsta) x y v igrac) (izdvojiSvaPolja (cdr vrsta) x (+ 1 y) v igrac))
        )
    )
)


(defun izdvojiSveVrste (tabla x y v igrac)
    (if (not tabla)
        NIL
        (append (izdvojiSvaPolja (car tabla) x y v igrac) (izdvojiSveVrste (cdr tabla) (+ 1 x) y v igrac))
    )
)

(defun izdvojiStanjaSusedi (stanje polje susedi)
    (if (not susedi)
        NIL
        (if (validanPotez
                stanje 
                (list (car polje) (cadr polje)) 
                (list (+ (car polje) (caar susedi)) (+ (cadr polje) (cadar susedi))) 
                (caddr polje)
            )
            (cons 
                (odigrajPotez 
                    stanje 
                    (list 
                        (list (vrstaUslovo (car polje)) (cadr polje)) 
                        (list (vrstaUslovo (+ (car polje) (caar susedi))) (+ (cadr polje) (cadar susedi))) 
                        (caddr polje)
                    )
                )
                (izdvojiStanjaSusedi stanje polje (cdr susedi))
            )
            (izdvojiStanjaSusedi stanje polje (cdr susedi))
        )
    )
)

;; POMOCNA
;; generise listu svih mogucih narednih stanja za zadato stanje
(defun izdvojiStanja_ (stanje izdvojeneVrste igrac susedi)
    (if (not izdvojeneVrste)
        NIL
        (append (izdvojiStanjaSusedi stanje (car izdvojeneVrste) susedi) (izdvojiStanja_ stanje (cdr izdvojeneVrste) igrac susedi))
    )
)

;; GLAVNA
;; wrapper za gornju funkciju
(defun izdvojiStanja (stanje)
    (setq susedi '((1 1) (1 -1) (-1 1) (-1 -1)))
    (setq izdvojeneVrste (izdvojiSveVrste (cdr stanje) 1 1 0 (caar stanje)))
    (izdvojiStanja_ stanje izdvojeneVrste (caar stanje) susedi)
)
