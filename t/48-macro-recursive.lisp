;; Macro with multiple unquotes
(defmacro swap-if (cond a b)
  `(if ,cond ,b ,a))

(prin1 (swap-if t 1 2))
(prin1 (swap-if nil 1 2))

;; Nested macro call
(defmacro double (x)
  `(+ ,x ,x))

(prin1 (double 5))