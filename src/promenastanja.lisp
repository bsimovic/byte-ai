;; POMOCNA
;; menja element na poziciji 'pozicija' u listi 'lista' elementom x
(defun zameniUListi (lista x pozicija)
    (append (izdvojiPrvaN lista pozicija) (list x) (nthcdr (+ 1 pozicija) lista))
)

;; GLAVNA
;; vraca novo stanje na osnovu prosledjenog poteza
(defun odigrajPotez (stanje potez)
    (if (not potez)
        NIL
        (progn
            (setq polje1 (vratiPoljeSlovoBroj (cdr stanje) (caar potez) (cadar potez)))
            (setq polje2 (vratiPoljeSlovoBroj (cdr stanje) (caadr potez) (cadadr potez)))
            (setq visina (caddr potez))
            (setq novoPolje1 (izdvojiPrvaN polje1 visina))
            (setq novoPolje2 (append (nthcdr visina polje1) polje2))

            (setq noviHeader
                (list
                    (if (equal (caar stanje) 'X)
                        'O
                        'X 
                    )
                    (if (= (length novoPolje2) 8)
                        (if (equal (car novoPolje2) 'X)
                            (list (+ 1 (caadar stanje)) (car (cdadar stanje)))
                            (list (caadar stanje) (+ 1 (car (cdadar stanje))))
                        )
                        (cadar stanje)
                    )
                )
            )

            (if (= (length novoPolje2) 8)
                (setq novoPolje2 NIL)
            )
            
            (setq novaVrsta1
                (zameniUListi
                    (nth (slovoUvrstu (caar potez)) stanje)
                    novoPolje1
                    (- (cadar potez) 1)      
                )
            )
            (setq novaVrsta2
                (zameniUListi
                    (nth (slovoUvrstu (caadr potez)) stanje)
                    novoPolje2
                    (- (cadadr potez) 1)      
                )
            )

            (setq noviRezultat
                (if (= (length novoPolje2) 8)
                    (progn
                        (setq novoPolje2 NIL)
                        (if (= (caar stanje) 'X)
                            (list (+ 1 (cadar stanje)))
                        )
                    )
                )
            )

            (zameniUListi
                (zameniUListi
                    (zameniUListi
                        stanje
                        noviHeader
                        0
                    )
                    novaVrsta1
                    (slovoUvrstu (caar potez))
                )
                novaVrsta2
                (slovoUvrstu (caadr potez))
            )
        )
    )
)