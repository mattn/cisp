(labels ((a (n) (* n 2))
		 (b (n) (+ (a n) 3)))
  (prin1 (b 10)))
(setq y 10)
(labels ((f (x) (prin1 y))) (let ((y 20)) (f 30)))
(defun g (x) (prin1 x))
(labels ((f (x) (g x))) (labels ((g (x) (prin1 (* x x)))) (f 10)))
(prin1 (labels nil))
(prin1 (labels nil 10))
(labels nil (prin1 10) (prin1 20))
(labels ((f nil (prin1 10) (prin1 20))) (f))
(prin1 (labels ((f nil)) (f)))

(setq a 1)
(labels ((f (a) (prin1 a))) (f 10) (prin1 a))
(prin1 a)
