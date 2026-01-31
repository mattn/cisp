;; Edge cases test

;; No-argument macro
(defmacro no-args-macro ()
  42)

(prin1 (no-args-macro))
;; Expected: 42

;; Variable arguments macro
(defmacro varargs-macro (&rest args)
  `(+ ,@args))

(prin1 (varargs-macro 1 2 3))
;; Expected: 6

;; Macro that returns multiple forms
(defmacro multi-form-macro (x)
  `(progn
     (prin1 "before")
     ,x
     (prin1 "after")))

(prin1 (multi-form-macro 100))
;; Expected: "before" 100 "after"