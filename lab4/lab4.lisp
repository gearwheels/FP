;       4.40
(defun prior (op)
    (cond 
        ((eq op '+) 1)
        ((eq op '-) 2)
        ((eq op '*) 3) 
        ((eq op '/) 4)
        (t 0)
    )
)

(defun non-associative (op)
    (or (eq op '-) (eq op '/))
)

(declaim (ftype (function (t t) t) g))

(defun f (op l)
    (if (cdr l) 
        (concatenate 'string (g (car l) op) " " (f op (cdr l)) " " (write-to-string op))
        (g (car l) op)
    )
)

(defun h (l external-op)
    (cond 
        ((= (list-length l) 2)
            (cond
                ((eq (car l) '-)
                    (concatenate 'string "0 " (f (car l) (cdr l)) " -"))
                ((eq (car l) '/)
                    (concatenate 'string "1 " (f (car l) (cdr l))" /"))
                (t 
                    (f (car l) (cdr l)) )
            )
        )
        (t (f (car l) (cdr l)))
    )
)

(defun g (l external-op) 
    (if (atom l) 
        (write-to-string l)
        (h l external-op)
    )
)

(defun form-to-postfix (l)
    (h l '+)
)


;; (form-to-postfix '(+ (* b b) (- (* 4 a c)))) 
;; (form-to-postfix '(* (* a b) (* c (* d e))))
;; (form-to-postfix '(/ (- b c d) a) )

;; (form-to-postfix '(- (+ A (- (* B C))) D) )
;; (form-to-postfix '(* (+ A B) (+ C D)) )
;; (form-to-postfix '(- (+ (+ A B) C) D) )
;; (form-to-postfix '(+ (* A B) (* C D)) )
;; (form-to-postfix '(* (/ a) c) )