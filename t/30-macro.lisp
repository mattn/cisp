(defmacro mac (x) (print x))
(mac 'a)
(defmacro negate (x) (cons '- (cons x nil)))
(print (negate 10))
