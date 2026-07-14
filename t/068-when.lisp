(when t
  (println "a")
  (println "b"))
(when nil
  (println "c")
  (println "d"))
(println (when t 1 2 3))
(println (when nil 1 2 3))
