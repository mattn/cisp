;; Simple recursion example
(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

(prin1 (fact 5))
(princ "\n")

;; Simple lambda example
(setq add1 (lambda (x) (+ x 1)))
(prin1 (funcall add1 10))
(princ "\n")