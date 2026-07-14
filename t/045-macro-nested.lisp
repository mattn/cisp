;; Nested macro expansion test
(defmacro outer-macro (x)
  `(inner-macro ,x))

(defmacro inner-macro (y)
  `(+ ,y 10))

(prin1 (outer-macro 5))
;; Expected: (+ 5 10) expands to 15