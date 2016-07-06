(print ((lambda (a b) (+ a b)) 3 4))
(setq plus (lambda (a b) (+ a b)))
(print (funcall plus 1 2))

; TODO: 落ちる
;(print (apply plus '(1 2)))
;(apply plus 1 '(2))
;(apply plus 1 2 ())
;(apply plus 1 2 nil)
