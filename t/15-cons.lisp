(prin1 (car (cons 123 456)))
(prin1 (cdr (cons 123 456)))
(prin1 (cons 1 2))
(prin1 (cons 1 (cons 2 3)))
(prin1 (cons 1 nil))
(prin1 (cons 1 (cons 2 (cons 3 nil))))
(prin1 (cons 1 '(10 20 30 40 50)))
(prin1 (apply 'cons '((+ 1 2) (+ 3 4))))
