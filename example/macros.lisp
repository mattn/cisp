;; Define a when macro (conditional execution)
(defmacro when (condition &rest body)
  `(if ,condition
       (progn ,@body)
       '()))

;; Define an unless macro (opposite of when)
(defmacro unless (condition &rest body)
  `(if (not ,condition)
       (progn ,@body)
       '()))

;; Define a let* macro for sequential binding
(defmacro let* (bindings &rest body)
  (if (null bindings)
      `(progn ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))))

;; Test the macros
(setq x 10)
(setq y 20)

(princ "When x > 5: ")
(when (> x 5)
  (princ "x is greater than 5, ")
  (princ "x = ")
  (prin1 x))
(princ "\n")

(princ "Unless y > 100: ")
(unless (> y 100)
  (princ "y is not greater than 100, ")
  (princ "y = ")
  (prin1 y))
(princ "\n")

(princ "Let* sequential binding: ")
(let* ((a 1)
       (b (+ a 1))
       (c (+ b 1)))
  (princ "a = ")
  (prin1 a)
  (princ ", b = ")
  (prin1 b)
  (princ ", c = ")
  (prin1 c))
(princ "\n")