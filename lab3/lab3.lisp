; 3.18
(defun extend-matrix (a v k)
  (let ((m (array-dimension a 0))
        (n (array-dimension a 1))
        (n1 (array-dimension v 0)))
    (let ((b (make-array (list m (1+ n)))))
      (dotimes (i m)
        (dotimes (j k)
          (setf (aref b i j) (aref a i j))))
      (loop
        :for i :below m
        :for x :below n1
        :do (setf (aref b i k) (aref v x)))
      (dotimes (i m)
        (loop :for j :from k :to (1- n) :do
          (setf (aref b i (1+ j)) (aref a i j))))
      b)))

(defun print-matrix (a &optional (format "~a"))
  (dotimes (i (array-dimension a 0))
    (dotimes (j (array-dimension a 1))
      (format t format (aref a i j)))
    (terpri)))


(defun mainFun (a v k format)
  (print-matrix a format)
  (terpri)
  (print-matrix (extend-matrix a v k) format))



;test_variables
(defvar m1 (make-array '(3 3) :initial-contents '((2 2 2) (0 1 0) (7 0 5))))
(defvar u1 #(3 3 3))
(defvar m2 (make-array '(2 4) :initial-contents '((0 0 0 0) (0 0 0 0))))
(defvar u2 #(1 1))
(defvar m3 (make-array '(3 4) :initial-contents '((0 0 0 0) (0 0 0 0) (0 0 0 0))))
(defvar u3 #(1 1 1))
(defvar m4 (make-array '(1 1) :initial-contents '((0))))
(defvar u4 #(1))
(defvar m5 (make-array '(4 2) :initial-contents '((0 0) (0 0) (0 0) (0 0))))
(defvar u5 #(1 1))