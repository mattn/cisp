;; Basic backquote with unquote
(prin1 `(a ,(+ 1 2) c))
;; Expected: (A 3 C)

;; Backquote with splice
(let ((x '(1 2 3)))
  (prin1 `(a ,@x d)))
;; Expected: (A 1 2 3 D)

;; Nested quote inside backquote
(prin1 `(a 'b c))
;; Expected: (A 'B C) or (A (QUOTE B) C)