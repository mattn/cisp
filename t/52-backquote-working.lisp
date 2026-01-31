;; Basic backquote examples (working cases)

;; 1. Simple backquote
(prin1 `(1 2 3))
;; => (1 2 3)

;; 2. Comma evaluation
(setq x 42)
(prin1 `(1 ,x 3))
;; => (1 42 3)

;; 3. @ splicing (list expansion)
(setq y '(2 3))
(prin1 `(1 ,@y 4))
;; => (1 2 3 4)

;; 4. Nested backquote
(setq a 10)
(setq b 20)
(prin1 `(,(+ a b)))
;; => (30)