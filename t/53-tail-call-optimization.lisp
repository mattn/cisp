; Test tail call optimization
; Deep recursion that would stack overflow without TCO
(defun countdown (n)
  (if (<= n 0)
      0
    (countdown (- n 1))))
(prin1 (countdown 10000))

; Tail recursive sum with accumulator
(defun sum-tail (n acc)
  (if (<= n 0)
      acc
    (sum-tail (- n 1) (+ acc n))))
(prin1 (sum-tail 100 0))

; Tail recursive factorial
(defun factorial-tail (n acc)
  (if (<= n 1)
      acc
    (factorial-tail (- n 1) (* n acc))))
(prin1 (factorial-tail 10 1))

; Mutual recursion (even/odd) - tail calls
(defun is-even (n)
  (if (= n 0)
      t
    (is-odd (- n 1))))
(defun is-odd (n)
  (if (= n 0)
      nil
    (is-even (- n 1))))
(prin1 (is-even 1000))
(prin1 (is-odd 1000))

; Tail recursive list length
(defun length-tail (lst acc)
  (if (null lst)
      acc
    (length-tail (cdr lst) (+ acc 1))))
(prin1 (length-tail '(1 2 3 4 5) 0))

; Tail recursive reverse
(defun reverse-tail (lst acc)
  (if (null lst)
      acc
    (reverse-tail (cdr lst) (cons (car lst) acc))))
(prin1 (reverse-tail '(1 2 3) nil))

; Test with labels - mutual tail recursion
(labels ((f (n) (if (<= n 0) 0 (g (- n 1))))
         (g (n) (if (<= n 0) 0 (f (- n 1)))))
  (prin1 (f 5000)))
