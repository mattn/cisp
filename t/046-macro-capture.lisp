;; Variable scope test - variable capture
(setq x 100)

(defmacro bad-macro (y)
  `(list x ,y))  ;; x should capture global variable but fails

(prin1 (bad-macro 5))
;; Expected: (100 5) but fails

;; Correct implementation using let
(defmacro good-macro (x y)
  `(let ((x-value ,x) (y-value ,y))
     (list x-value y-value)))

(prin1 (good-macro 10 20))
;; Expected: (10 20)