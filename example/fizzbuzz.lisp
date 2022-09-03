(defun fizzbuzz ()
  (dotimes (i 100)
    (princ (format nil "~A~%"
     (cond
      ((= 0 (mod i 15)) "FizzBuzz")
      ((= 0 (mod i 3)) "Fizz")
      ((= 0 (mod i 5)) "Buzz")
      ('t i))))))

(fizzbuzz)
