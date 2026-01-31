;; Map function - apply function to each element of a list
(defun my-map (func lst)
  (if (null lst)
      '()
      (cons (funcall func (car lst))
            (my-map func (cdr lst)))))

;; Filter function - return elements that satisfy predicate
(defun my-filter (pred lst)
  (if (null lst)
      '()
      (if (funcall pred (car lst))
          (cons (car lst) (my-filter pred (cdr lst)))
          (my-filter pred (cdr lst)))))

;; Reduce function - accumulate over list
(defun my-reduce (func lst acc)
  (if (null lst)
      acc
      (my-reduce func (cdr lst) (funcall func acc (car lst)))))

;; Test the functions
(setq numbers '(1 2 3 4 5 6 7 8 9 10))

(princ "Original list: ")
(prin1 numbers)
(princ "\n")

(princ "Map (square): ")
(prin1 (my-map (lambda (x) (* x x)) numbers))
(princ "\n")

(princ "Filter (even): ")
(prin1 (my-filter (lambda (x) (= 0 (mod x 2))) numbers))
(princ "\n")

(princ "Reduce (sum): ")
(prin1 (my-reduce (lambda (acc x) (+ acc x)) numbers 0))
(princ "\n")