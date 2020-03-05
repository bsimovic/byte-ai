;; GLAVNA
;; zapocinje igru izmedju dva igraca na tabli velicine n
(defun startPvP (n)
    (setq stanje (byteStart n))
    (loop 
        (terpri)
        (format t "~a" stanje)
        (terpri)
        (format t "Trenutno stanje igre: ")
        (terpri)
        (crtajStanje stanje)
        (terpri)
        (format t "Unesi x1: ")
        (setq x1 (char (string (read)) 0))
        (format t "Unesi y1: ")
        (setq y1 (read))
        (format t "Unesi x2: ")
        (setq x2 (char (string (read)) 0))
        (format t "Unesi y2: ")
        (setq y2 (read))
        (format t "Unesi visinu: ")
        (setq visina (read))
        (terpri)
        
        (if (validanPotez stanje (list (slovoUvrstu x1) y1) (list (slovoUvrstu x2) y2) visina)
            (setq stanje (odigrajPotez stanje (list (list x1 y1) (list x2 y2) visina)))
            (format t "POTEZ NIJE VALIDAN")
        )

        (setq kraj (proveriKraj stanje))
        (if kraj
            (progn 
                (format t "Igra se zavrsila: ")
                (format t "Pobednik je ~a." kraj)
                (return kraj)
            )
        )
    )
)