(defun g (m) 2)
(flet ((f (n) (+ n (g 2))) (g (m) 1))
  (print (f 5)))
