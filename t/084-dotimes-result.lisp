(setq acc nil)
(println (dotimes (i (+ 2 1) acc) (setq acc (cons i acc))))
(println (dotimes (i 3 i)))
(println (dotimes (i 0 'done) (println 'never)))
