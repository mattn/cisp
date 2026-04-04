(prin1 (call/cc (lambda (k) (+ 1 2))))
(prin1 (+ 1 (call/cc (lambda (k) (funcall k 41) 100))))
(prin1 (call/cc (lambda (k) (funcall 'funcall k 7) 9)))
