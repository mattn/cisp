;; Create a list with cons cells
(defun create-list (n)
  (if (= n 0)
      '()
      (cons n (create-list (- n 1)))))

;; List operations
(defun list-length (lst)
  (if (null lst)
      0
      (+ 1 (list-length (cdr lst)))))

(defun list-reverse (lst)
  (if (null lst)
      '()
      (nconc (list-reverse (cdr lst)) (list (car lst)))))

(defun list-map (func lst)
  (if (null lst)
      '()
      (cons (funcall func (car lst))
            (list-map func (cdr lst)))))

;; Test list operations
(princ "Create list 1-5: ")
(prin1 (create-list 5))
(princ "\n")

(princ "Length: ")
(prin1 (list-length (create-list 5)))
(princ "\n")

(princ "Squared: ")
(prin1 (list-map (lambda (x) (* x x)) (create-list 5)))
(princ "\n")

(princ "Reversed: ")
(prin1 (list-reverse (create-list 5)))
(princ "\n")