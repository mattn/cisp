;; Simpler self-referential examples
(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

(princ "Factorial of 5: ")
(prin1 (fact 5))
(princ "\n")

;; Y combinator style (simplified)
(defun make-counter ()
  (let ((count 0))
    (lambda () (setq count (+ count 1)))))

(setq counter (make-counter))
(princ "Counter: ")
(prin1 (funcall counter))
(princ ", ")
(prin1 (funcall counter))
(princ ", ")
(prin1 (funcall counter))
(princ "\n")

;; Function that returns itself
(defun self-returner ()
  'self-returner)

(princ "Function that returns itself: ")
(prin1 (self-returner))
(princ "\n")