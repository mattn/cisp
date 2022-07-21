(prin1 ((lambda (a b) (+ a b)) 3 4))
(setq plus (lambda (a b) (+ a b)))
(prin1 (funcall plus 1 2))
(prin1 (apply plus '(2 3)))

(setq a 10)
(prin1 ((lambda (a) a) 20))
(prin1 a)
