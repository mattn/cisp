(defmacro mac (x) (prin1 x))
(mac 'a)
(defmacro negate (x) (cons '- (cons x nil)))
(prin1 (negate 10))

(defmacro m nil)
(prin1 (m))

(defmacro m (x) (prin1 x) (list '+ x 'y))
(setq y 1)
(prin1 (m (+ 10 20)))

(defun x (y) (defmacro m (x) (list '+ x y)))
(x 10)
(prin1 (m 20))

(defmacro m (x) (list '+ x 'x))
(setq x 10)
(prin1 (m 20))
