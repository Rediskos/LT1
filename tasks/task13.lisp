(defun check-for-reps (target *array*)
  (cond
    ;;если не осталось елементов то вернуть NIL
    ((null *array*) NIL)
    ;;если голова массива совпала с целью вернуть T
    ((equal target (car *array*)) T)
    ;;если голова массива не совпала с целью то продолжить с хвостом
    (t (check-for-reps target (cdr *array*)))
    ))

(defun remove-reps (*array*)
  ((lambda (car-ar cdr-ar)
     (cond
       ((null cdr-ar) *array*)
       ((equal (check-for-reps car-ar cdr-ar) NIL) (cons car-ar (remove-reps cdr-ar)))
       (t (remove-reps cdr-ar))))
   (car *array*) (cdr *array*)
   )
  )

(remove-reps '(1 2 5 3 4 5))

(terpri)
(princ "Никулин - задание 13")
(terpri)
(princ "Тест 1, n=(1 2 5 3 4 5): ")
(princ (remove-reps '(1 2 5 3 4 5)))

(terpri)
(princ "Тест 2, n=(1 1 1 2 2 2): ")
(princ (remove-reps '(1 1 1 2 2 2)))

(terpri)
(princ "Тест 3, n=(\"a\" 1 \"a\" 1 \"b\"): ")
(princ (remove-reps '("a" 1 "a" 1 "b")))

