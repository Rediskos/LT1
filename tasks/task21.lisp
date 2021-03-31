(defun check-for-reps (target *array*)
  (cond
    ;;если не осталось елементов то вернуть NIL
    ((null *array*) NIL)
    ;;если голова массива совпала с целью вернуть T
    ((equal target (car *array*)) T)
    ;;если голова массива не совпала с целью то продолжить с хвостом
    (t (check-for-reps target (cdr *array*)))
    ))


(defun del-first-occur (*array* &optional (*trace* NIL))
  ((lambda (car-ar cdr-ar)
     (cond
       ((null *array*) NIL)
       ((check-for-reps car-ar *trace*) (cons car-ar (del-first-occur cdr-ar *trace*)))
       (t (del-first-occur cdr-ar (cons car-ar *trace*)))))
   (car *array*) (cdr *array*)
   )
  )

(terpri)
(princ "Никулин - задание 21")
(terpri)
(princ "Тест 1, n=(1 2 5 3 4 5): ")
(princ (del-first-occur '(1 2 5 3 4 5)))
(terpri)
(princ "Тест 2, n=(1 1 2 2 3 3): ")
(princ (del-first-occur '(1 1 2 2 3 3)))
(terpri)
(princ "Тест 3, n=(\"a\" 1 \"a\" \"b\" 1): ")
(princ (del-first-occur '("a" 1 "a" "b" 1)))
