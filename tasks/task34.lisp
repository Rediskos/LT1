

(defun УДАЛИТЬ (target set)
  ((lambda (f-set cdr-set)
     (cond
       ((null set) NIL)
       ((equal target f-set) (УДАЛИТЬ target cdr-set))
       (t (cons f-set (УДАЛИТЬ target cdr-set)))))
   (first set) (cdr set))
  )

(defun РАВЕНСТВО-МНОЖЕСТВ (set1 set2)
  ((lambda (f-set1 set1-null set2-null)
     (if (equal set1-null set2-null)
       (if set1-null
         T
         (РАВЕНСТВО-МНОЖЕСТВ (cdr set1) (УДАЛИТЬ f-set1 set2)))
       NIL)
     )
   (first set1) (null set1) (null set2))
  )

(РАВЕНСТВО-МНОЖЕСТВ '(1 2 3) '(1 2 3 4))

(terpri)
(princ "Никулин - задание 34")
(terpri)
(princ "Тест 1, set1=(1 2 3), set2=(1 2 3 4): ")
(princ (РАВЕНСТВО-МНОЖЕСТВ '(1 2 3) '(1 2 3 4)))
(terpri)
(princ "Тест 2, set1=(1 2 3), set2=(4 3 2 1): ")
(princ (РАВЕНСТВО-МНОЖЕСТВ '(1 2 3) '(4 3 2 1)))
(terpri)
(princ "Тест 3, set1=(1 2 3), set2=(1 2 3): ")
(princ (РАВЕНСТВО-МНОЖЕСТВ '(1 2 3) '(1 2 3)))
(terpri)
(princ "Тест 4, set1=(1 2 3), set2=(3 2 1): ")
(princ (РАВЕНСТВО-МНОЖЕСТВ '(1 2 3) '(3 2 1)))
