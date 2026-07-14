;; Basic variable passing test
(defmacro simple-arg-test (x)
  `(+ ,x 10))

(prin1 (simple-arg-test 5))
;; Expected: 15

;; Multiple arguments test
(defmacro multi-arg-test (x y)
  `(* ,x ,y))

(prin1 (multi-arg-test 3 4))
;; Expected: 12