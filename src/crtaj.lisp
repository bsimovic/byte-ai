;; POMOCNA
;; crta liniju tacaka duzine n
(defun crtajCrnuLiniju (lista n)
	(if (> n 0)
		(progn
			(crtajCrnuLiniju (cdr lista) (- n 1))
			(cond
				((equal (car lista) NIL) (format t "."))
				((equal (car lista) 'X) (format t "X"))
				((equal (car lista) 'O) (format t "O"))
			)
		)
	)
)

;; POMOCNA
;; crta liniju razmaka duzine n
(defun crtajBeluLiniju (n)
	(if (> n 1)
		(crtajBeluLiniju (- n 1))
	)
	(format t " ")
)

;; POMOCNA
;; okrece polje i prosiruje polje NIL-ovima dok ne stigne n
;; (prosiriPolje '(X X O) 8) => (NIL NIL NIL NIL NIL O O X)
(defun prosiriPolje (polje n)
	(if (= (length polje) n)
		(reverse polje)
		(cons NIL (prosiriPolje polje (- n 1)))
	)
)

;; POMOCNA
;; izdvaja prva n elemenata iz liste
(defun izdvojiPrvaN (lista n)
	(if (= n 0)
		NIL
		(cons (car lista) (izdvojiPrvaN (cdr lista) (- n 1)))
	)
)

;; POMOCNA
;; deli polje na n dela
;; (izdvojiDelove '(NIL NIL NIL NIL X O O X) 3) => ((NIL NIL NIL) (NIL X O) (O O X))
(defun izdvojiDelove (polje n)
	(if (not polje)
		NIL
		(cons (izdvojiPrvaN polje n) (izdvojiDelove (nthcdr n polje) n))
	)
)

;; POMOCNA
;; crta jednu horizontalnu liniju vrste
(defun crtajDeoVrste (vrsta velicinaDelaVrste brojDela)
	(if	vrsta
		(progn
			(if (equal (car vrsta) 'BELO)
				(crtajBeluLiniju velicinaDelaVrste)
				(crtajCrnuLiniju (nth brojDela (izdvojiDelove (car vrsta) velicinaDelaVrste)) velicinaDelaVrste)
			)
			(crtajDeoVrste (cdr vrsta) velicinaDelaVrste brojDela)
		)
	)
)

;; POMOCNA
;; crta cela vrstu sa slovom ispred
(defun crtajVrstu (vrsta velicinaDelaVrste brojDela slovo)
	(if (>= brojDela 0)
		(progn
			(if (= brojDela 1)
				(format t "~a " slovo)
				(format t "  ")
			)
			(crtajDeoVrste vrsta velicinaDelaVrste brojDela)
			(terpri)
			(crtajVrstu vrsta velicinaDelaVrste (- brojDela 1) slovo)
		)
	)
)

;; POMOCNA
;; crta celu tablu sa slovima
(defun crtajTablu (stanje velicinaDelaVrste slovo)
	(if stanje
		(progn
			(crtajVrstu (car stanje) velicinaDelaVrste 2 slovo)
			(crtajTablu (cdr stanje) velicinaDelaVrste (code-char (+ (char-code slovo) 1)))
		)
	)
)

;; POMOCNA
;; crta brojeve od 1 do n sa razmakom velicinaDelaVrste
(defun crtajBrojeve (n velicinaDelaVrste)
	(if (> n 1)
		(crtajBrojeve (- n 1) velicinaDelaVrste)
	)
	(crtajBeluLiniju (floor (/ velicinaDelaVrste 2)))
	(format t "~a" n)
	(crtajBeluLiniju (floor (/ velicinaDelaVrste 2)))
)

;; GLAVNA
;; crta kompletno stanje ukljucujuci tablu, koji je igrac na potezu i trenutni rezultat
(defun crtajStanje (stanje)
	(setq n (length (cdr stanje)))
	(if (= n 8)
		(setq velicinaDelaVrste 3)
		(setq velicinaDelaVrste 5)
	)
	(crtajBeluLiniju 2)
	(crtajBrojeve n velicinaDelaVrste)
	(terpri)
	(crtajTablu (cdr stanje) velicinaDelaVrste #\A)
	(terpri)
	(format t "~a je na potezu~%Rezultat je ~a:~a" (caar stanje) (caadar stanje) (car (cdadar stanje)))
)

;; (crtajStanje '((X (0 0)) (NIL BELO NIL BELO NIL BELO NIL BELO NIL BELO) (BELO (X) BELO (X) BELO (X) BELO (X) BELO (X)) ((O) BELO (O) BELO (O) BELO (O) BELO (O) BELO) (BELO (X) BELO (X) BELO (X) BELO (X) BELO (X)) ((O) BELO (O) BELO (O) BELO (O) BELO (O) BELO) (BELO (X) BELO (X) BELO (X) BELO (X) BELO (X)) ((O) BELO (O) BELO (O) BELO (O) BELO (O) BELO) (BELO (X) BELO (X) BELO (X) BELO (X) BELO (X)) ((O) BELO (O) BELO (O) BELO (O) BELO (O) BELO) (BELO NIL BELO NIL BELO NIL BELO NIL BELO NIL)))