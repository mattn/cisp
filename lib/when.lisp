(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
