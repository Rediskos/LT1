(defun ВЫЧИСЛИ (*expr*)
  (cond
    ((null *expr*) nil)
    ((atom  *expr*) *expr*)
    (t (eval (cons (second *expr*) (cons (ВЫЧИСЛИ (first *expr*))
                                (list (ВЫЧИСЛИ (third *expr*))))))))
  )

(ВЫЧИСЛИ '((-2 + 4) * 3))

(terpri)
(princ "Никулин - задание 30")
(terpri)
(princ "Тест 1, n=((-2 + 4) * 3): ")
(princ (ВЫЧИСЛИ '((-2 + 4) * 3)))
(terpri)
(princ "Тест 2, n=((-2 + 4) * (20 + (30 - 40))): ")
(princ (ВЫЧИСЛИ '((-2 + 4) * (20 + (30 - 40)))))
(terpri)
