(defun task-4 (n &optional (cnt '0))
  (cond
    ((= cnt n) NIL)
    (t (cons (+ cnt 1) (task-4 n (+ cnt 1))))
    )
  )

(terpri)
(princ "Никулин - задание 4")
(terpri)
(princ "Тест 1, n=5: ")
(princ (task-4 5))

(terpri)
(princ "Тест 2, n=8: ")
(princ (task-4 8))

(terpri)
(print "Тест 3, n=20: ")
(princ (task-4 20))
