(setq a 1)
(defun x () (setq a 2))
(x)
(prin1 a)
(defun y ()
  (let ((a 3)) (prin1 a))
  (prin1 a)
)
(y)
(prin1 a)
(let ((x 1)))
(let ((x 1) (y 2)) (prin1 x) (prin1 y))
(setq foo 1)
(let* ((foo 2) (bar foo))
  (prin1 bar) (prin1 foo))
(let ((foo 3) (bar foo))
  (prin1 bar) (prin1 foo))
(prin1 (let nil))
(prin1 (let (x) x))
(setq x 100)
(setq y 200)
(prin1 (let (x y) (setq x 10) (setq y 20) (+ x y)))
(prin1 (let* ((x (+ 1 2)) (y (+ x x))) (+ x y)))
(prin1 x)
(prin1 y)
(setq x (lambda (a) a))
(prin1 (let  ((x (lambda (a) (funcall x a)))) (funcall x 1)))
(prin1 (let* ((x (lambda (a) (funcall x a)))) (funcall x 2)))
