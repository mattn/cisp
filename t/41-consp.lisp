(prin1 (consp nil))
(prin1 (consp t))
(prin1 (consp '(1 . 2)))
(prin1 (consp '(1 2)))
(prin1 (consp 1))
(prin1 (consp 1.0))
(prin1 (consp "string"))
(prin1 (consp 'x))
(prin1 (consp ''x))
(prin1 (consp '`x))
(prin1 (consp (lambda (x) x)))
(setq x '(+ 1 2))
(prin1 (consp x))
(prin1 (apply 'consp '((+ 1 2))))
