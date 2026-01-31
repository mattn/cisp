;; Complex backquote syntax examples (problematic cases)

;; 1. Multiple comma nesting
(setq x 5)
(prin1 ``((,,x) ,x))
;; Expected: ((5) 5)
;; Actual: Parse error

;; 2. Deep nesting with commas
(setq a 1)
(setq b 2)
(prin1 `(`(,,a ,b)))
;; Expected: (`(1 2)) or (1 2)
;; Actual: Error

;; 3. Mixing @ and commas
(setq lst1 '(1 2))
(setq lst2 '(3 4))
(prin1 `(,@lst1 ,@lst2))
;; Expected: (1 2 3 4)

;; 4. Complex comma-at (,@) usage
(prin1 `(let ((x 10)) ,@(list x x)))
;; Expected: (let ((x 10)) 10 10)