(prin1 (null nil))
(prin1 (null t))
(setq x nil)
(prin1 (null x))
(prin1 (null 'x))
(prin1 (apply 'null '(x)))
(prin1 (apply 'null (cons x nil)))
