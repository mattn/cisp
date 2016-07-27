(defmacro first (x) (car x))
(defmacro second (x) (car (cdr x)))
(defmacro third (x) (car (cdr (cdr x))))
(defmacro fourth (x) (car (cdr (cdr (cdr x)))))
