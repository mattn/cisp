(prin1 (apply '+ '(1 2 3 4 5 6 7 8 9 10)))
(prin1 (apply '+ 1 2 3 4 5 6 7 8 9 10 nil))
(prin1 (apply '+ 1 2 3 4 5 '(6 7 8 9 10)))
(prin1 (apply '+ nil))
(prin1 (apply '+ (+ 1 2 3) (+ 4 5 6) (+ 7 8 9) (cons 10 nil)))
(prin1 (apply 'funcall (list '+ (+ 1 2 3) (+ 4 5 6) (+ 7 8 9) 10)))
(prin1 (apply 'funcall '+ (+ 1 2 3) (+ 4 5 6) (+ 7 8 9) (cons 10 nil)))
(setq prin1 10)
(apply 'prin1 '(20))
(apply 'prin1 '((+ 1 2)))
(prin1 (apply 'car '((1 2))))
(prin1 (apply 'cdr '((1 2))))
