;; POMOCNA
;; menja slovo u broj vrste
(defun slovoUvrstu (slovo)
    (- (char-code slovo) 64)
)

(defun vrstaUslovo (vrsta)
    (code-char (+ vrsta 64))
)

;; POMOCNA
;; vraca polje za zadatu vrstu i kolonu gde je vrsta tipa char
(defun vratiPoljeSlovoBroj (tabla vrsta kolona)
    (setq vrstaList (nth (- (char-code (char (string vrsta) 0)) 65) tabla))
    (if (equal vrstaList 'BELO)
        'BELO
        (nth (- kolona 1) vrstaList)
    )
)

;; POMOCNA
;; vraca polje za zadatu vrstu i kolonu gde je vrsta broj
(defun vratiPoljeBrojBroj (tabla vrsta kolona)
    (setq vrstaList (nth (- vrsta 1) tabla))
    (if (equal vrstaList 'BELO)
        'BELO
        (nth (- kolona 1) vrstaList)
    )
)   

;; POLU-POMOCNA
;; proverava da li su koordinate x2 y2 unutar table velicine n
(defun proveriUTabli (x2 y2 n)
    (and 
        (> x2 0) 
        (<= x2 n) 
        (> y2 0)
        (<= y2 n)
    )
)

;; POLU-POMOCNA
;; proverava da li su polja (x1 y1) i (x2 y2) dijagnolani susedi
(defun proveriKorak (x1 y1 x2 y2)
    (and 
        (= (abs (- x1 x2)) 1) 
        (= (abs (- y1 y2)) 1)
    )
)

;; POMOCNA
;; proverava da li je moguce spojiti stek na polju1 sa poljem2 sa visine visina
(defun proveriSpajanje (polje1 polje2 visina)
    (and 
        (<= visina (- (length polje1) 1)) ;; da li postoji data visina
        (< visina (length polje2)) ;; da li je visina manja od visine polja 2
        (<= (+ (- (length polje1) visina) (length polje2)) 8) ;; da li je rezultujuci stek manji od 8
    )
)

;; POMOCNA
;; prosiruje funkciju proveri spajanje tako sto proverava da li je moguce spajanje za odredjenog igraca
(defun proveriSpajanjeZaIgraca (igrac polje1 polje2 visina)
    (and
        (equal (nth visina (reverse polje1)) igrac)
        (proveriSpajanje polje1 polje2 visina)
    )
)

;; POLU-POMOCNA
;; proverava da li postoji spajajuci potez izmedju polja1 i polja2 za igraca igrac (visina je pomocni arg)
(defun moguceSpajanje (igrac polje1 polje2 visina)
    (if (>= visina (length polje1))
        NIL
        (if (equal (nth visina (reverse polje1)) igrac)
            (or 
                (proveriSpajanje polje1 polje2 visina)
                (moguceSpajanje igrac polje1 polje2 (+ visina 1))
            )
            (moguceSpajanje igrac polje1 polje2 (+ visina 1))
        )
    )
)

;; POMOCNA
;; proverava da li je na dnu steka zadati igrac
(defun proveriDno (igrac polje)
    (or
        (= (length polje) 1)
        (equal (car (reverse polje)) igrac)
    )  
)

;; POMOCNA
;; odredjuje koja polja vrste nisu prazna
(defun izdvojiKoordIzVrste (vrsta x y)
    (if (not vrsta)
        NIL
        (if (or (equal (car vrsta) 'BELO) (not (car vrsta)))
            (izdvojiKoordIzVrste (cdr vrsta) x (+ 1 y))
            (cons (list x y) (izdvojiKoordIzVrste (cdr vrsta) x (+ 1 y)))
        )
    )
)

;; POMOCNA
;; odredjuje koja polja iz cele table nisu prazna
(defun izdvojiKoordIzTable (tabla x)
    (if (not tabla)
        NIL
        (append (izdvojiKoordIzVrste (car tabla) x 1) (izdvojiKoordIzTable (cdr tabla) (+ 1 x)))
    )
)

;; POMOCNA
;; odredi ka kojim poljim bi se bilo valdino kretati sa polja (x1 y1)
(defun izdvojiValidneKoord (tabla igrac koord x1 y1)
    (if (not koord)
        NIL
        (if (or 
                (not (moguceSpajanje igrac (vratiPoljeBrojBroj tabla x1 y1) (vratiPoljeBrojBroj tabla (caar koord) (cadar koord)) 0))
                (and
                    (= x1 (caar koord))
                    (= y1 (cadar koord))
                )
            )
            (izdvojiValidneKoord tabla igrac (cdr koord) x1 y1)
            (cons (car koord) (izdvojiValidneKoord tabla igrac (cdr koord) x1 y1))
        )
    )
)

;; POMOCNA
;; nalazi daljinu u brojevima poteza izmedju polja (x1 y1) (x2 y2)
(defun nadjiJednuDaljinu (x1 y1 x2 y2)
     (max 
        (abs (- x2 x1)) 
        (abs (- y2 y1))
    )
)

;; POMOCNA
;; nalazi sve daljene izmedju polja (x1 y1) i svih polja unutar koord
(defun nadjiDaljine (koord x1 y1)
    (if (not koord)
        NIL
        (cons 
            (nadjiJednuDaljinu x1 y1 (caar koord) (cadar koord))
            (nadjiDaljine (cdr koord) x1 y1)
        )
    )
)

;; POMOCNA
;; izdvaja koordinate polja koja su na minimalnoj daljini od (x1 y1)
(defun izdvojiKoordMinimume (minimum koord x1 y1)
    (if (not koord)
        NIL
        (if (= minimum (nadjiJednuDaljinu x1 y1 (caar koord) (cadar koord)))
            (cons (car koord) (izdvojiKoordMinimume minimum (cdr koord) x1 y1))
            (izdvojiKoordMinimume minimum (cdr koord) x1 y1)
        )
    )
)

;; POMOCNA
;; odredjuje da li je korak na (x2 y2) ka najblizem mogucem steku
(defun priblizavaSe (minimum koordMinimum x2 y2)
    (if (not koordMinimum)
        NIL
        (or
            (< (nadjiJednuDaljinu x2 y2 (caar koordMinimum) (cadar koordMinimum)) minimum)
            (priblizavaSe minimum (cdr koordMinimum) x2 y2)
        )
    )
)

;; POLU-POMOCNA
;; proverava da li je korak sa (x1 y1) na (x2 y2) validan potez za igraca igrac sto se tice udaljenosti do najblizeg steka
(defun proveriDaljinu (tabla igrac x1 y1 x2 y2)
    (setq validneKoord (izdvojiValidneKoord tabla igrac (izdvojiKoordIzTable tabla 1) x1 y1))
    (setq daljine (nadjiDaljine validneKoord x1 y1))
    (if daljine
        (progn
            (setq minimum (reduce #'min daljine))
            (setq koordMinimum (izdvojiKoordMinimume minimum validneKoord x1 y1))
            (priblizavaSe minimum koordMinimum x2 y2)
        )
        NIL
    )
)

;; GLAVNA
;; proverava da li je korak sa koordPolje1 na koordPolje2 sa visine visina validan potez za igraca koji je na potezu
;; ako nije vraca NIL, ako jeste vraca format poteza
(defun validanPotez (stanje koordPolje1 koordPolje2 visina)
    (if (and
            (proveriUTabli (car koordPolje2) (cadr koordPolje2) (length (cdr stanje)))
            (proveriKorak (car koordPolje1) (cadr koordPolje1) (car koordPolje2) (cadr koordPolje2))
            (if (vratiPoljeBrojBroj (cdr stanje) (car koordPolje2) (cadr koordPolje2))
                (proveriSpajanjeZaIgraca (caar stanje) (vratiPoljeBrojBroj (cdr stanje) (car koordPolje1) (cadr koordPolje1)) (vratiPoljeBrojBroj (cdr stanje) (car koordPolje2) (cadr koordPolje2)) visina)
                (proveriDno (caar stanje) (vratiPoljeBrojBroj (cdr stanje) (car koordPolje1) (cadr koordPolje1)))
            )
            (proveriDaljinu (cdr stanje) (caar stanje) (car koordPolje1) (cadr koordPolje1) (car koordPolje2) (cadr koordPolje2))
        )
        (list koordPolje1 koordPolje2 visina)
        NIL
    )
)

;; (trace izdvojiValidneKoord)
;; (trace length)
;; (write (validanPotez '((X (0 0)) (NIL BELO NIL BELO NIL BELO NIL BELO) (BELO (X) BELO NIL BELO NIL BELO NIL) (NIL BELO NIL BELO NIL BELO NIL BELO) (BELO NIL BELO NIL BELO NIL BELO NIL) (NIL BELO NIL BELO NIL BELO (O) BELO) (BELO NIL BELO NIL BELO NIL BELO NIL) (NIL BELO NIL BELO NIL BELO NIL BELO) (BELO (X) BELO (X) BELO NIL BELO NIL)) '(2 2) '(1 3) 0))