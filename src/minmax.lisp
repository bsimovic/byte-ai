;; POMOCNE
;; broje sve plocice na vrhu za odredjenog igraca
(defun prebrojiSveUVrsti (vrsta igrac)
    (if (not vrsta)
        0
        (if (and (not (equal (car vrsta) 'BELO)) (equal (caar vrsta) igrac))
            (+ 1 (prebrojiSveUVrsti (cdr vrsta) igrac))
            (prebrojiSveUVrsti (cdr vrsta) igrac)
        )
    )
)

(defun prebrojiSveUTabli (tabla igrac)
    (if (not tabla)
        0
        (+
            (prebrojiSveUVrsti (car tabla) igrac) 
            (prebrojiSveUTabli (cdr tabla) igrac)
        )
    )
)

;; PRIVREMENA FUNKCIJA
;; hard-kodovana procena stanja
;; korisnost: (n - m + (skor_m * 10) - (skor_n * 10)) 
;; gde je:
;; n - broj plocica na vrhu za protivnika
;; m - broj plocica na vrhu za igraca koji je na potezu
(defun procenaStanja (stanje)
    (progn
        (setq naPotezu 'X)
        (setq protivnik 'O)
        (setq skor_naPotezu (car (cdadar stanje)))
        (setq skor_protivnik (caadar stanje))
    )
    (+ 
        (prebrojiSveUTabli (cdr stanje) protivnik)
        (* skor_naPotezu 10)
        (- (prebrojiSveUTabli (cdr stanje) naPotezu))
        (- (* skor_protivnik 10))
    )
)

(defun spojiStanjaIVrednosti (stanja)
    (if (not stanja)
        NIL
        (cons (list (car stanja) (procenaStanja (car stanja))) (spojiStanjaIVrednosti (cdr stanja)))
    )
)

;; POMOCNA
;; prodjiDecuAlfa, prodjiDecuBeta, maxStanje, minStanje, prvaDubinaMax, prvaDubinaMin implementiraju alfa beta odsecanje
(defun prodjiDecuAlfa (deca dubina alfa beta value)
    (let*
        (
            (
                noviValue
                (max
                    (minStanje (car deca) (- dubina 1) alfa beta)
                    value
                )
            )
            (
                novoAlfa
                (max
                    alfa
                    noviValue
                )
            )
        )
        (cond 
            ((>= novoAlfa beta) noviValue)
            ((not (cdr deca)) noviValue)
            (t (prodjiDecuAlfa (cdr deca) dubina novoAlfa beta noviValue))
        )
    )
)

(defun prodjiDecuBeta (deca dubina alfa beta value)
    (let*
        (
            (
                noviValue
                (min
                    (maxStanje (car deca) (- dubina 1) alfa beta)
                    value
                )
            )
            (
                novoBeta
                (min
                    beta
                    noviValue
                )
            )
        )
        (cond
            ((>= alfa novoBeta) noviValue)
            ((not (cdr deca)) noviValue)
            (t (prodjiDecuBeta (cdr deca) dubina alfa novoBeta noviValue))
        )
    )
    
)

(defun maxStanje (stanje dubina alfa beta)
    (let
        ((deca (spojiStanjaIVrednosti (izdvojiStanja (car stanje)))))
        (if (or (zerop dubina) (null deca))
            (cadr stanje)
            (prodjiDecuAlfa deca dubina alfa beta -1000)
        )
    )
)

(defun minStanje (stanje dubina alfa beta)
    (let
        ((deca (spojiStanjaIVrednosti (izdvojiStanja (car stanje)))))
        (if (or (zerop dubina) (null deca))
            (cadr stanje)
            (prodjiDecuBeta deca dubina alfa beta 1000)
        )
    )
)

(defun prvaDubinaMax (deca dubina max i maxind)
    (if (not deca)
        maxind
        (progn
            (let
                ((val (maxStanje (car deca) (- dubina 1) (- 1000) 1000)))
                (if (> val max)
                    (prvaDubinaMax (cdr deca) dubina val (+ i 1) i)
                    (prvaDubinaMax (cdr deca) dubina max (+ i 1) maxind)
                )
            )   
        )
    )
)

(defun prvaDubinaMin (deca dubina min i minind)
    (if (not deca)
        minind
        (progn
            (let
                ((val (minStanje (car deca) (- dubina 1) (- 1000) 1000)))
                (if (< val min)
                    (prvaDubinaMin (cdr deca) dubina val (+ i 1) i)
                    (prvaDubinaMin (cdr deca) dubina min (+ i 1) minind)
                )
            )   
        )
    )
)

;; GLAVNA
;; implementira minmax algoritam do dubine dubina
(defun minmax (stanje dubina mojPotez)
   (let
        ((deca (spojiStanjaIVrednosti (izdvojiStanja stanje))))
        (if mojPotez
            (nth (prvaDubinaMax deca dubina -1000 0 0) deca)
            (nth (prvaDubinaMin deca dubina 1000 0 0) deca)
        )
    )
)
