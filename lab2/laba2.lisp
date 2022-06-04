;       2.39

(defun split (list)
        (cond ((null list) nil)
                ((atom (car list)) (cons (car list) (split (cdr list))))
                (t (append (split (car list)) (split (cdr list))))))
