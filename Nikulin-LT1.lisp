;;задание 4

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



;;задание 13

(defun check-for-reps (target array)
  (cond
    ;;если не осталось елементов то вернуть NIL
    ((null array) NIL)
    ;;если голова массива совпала с целью вернуть T
    ((equal target (car array)) T)
    ;;если голова массива не совпала с целью то продолжить с хвостом
    (t (check-for-reps target (cdr array)))
    ))

(defun remove-reps (array)
  ((lambda (car-ar cdr-ar)
     (cond
       ((null cdr-ar) array)
       ((equal (check-for-reps car-ar cdr-ar) NIL) (cons car-ar (remove-reps cdr-ar)))
       (t (remove-reps cdr-ar))))
   (car array) (cdr array)
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



;;задание 21

(defun check-for-reps (target array)
  (cond
    ;;если не осталось елементов то вернуть NIL
    ((null array) NIL)
    ;;если голова массива совпала с целью вернуть T
    ((equal target (car array)) T)
    ;;если голова массива не совпала с целью то продолжить с хвостом
    (t (check-for-reps target (cdr array)))
    ))


(defun del-first-occur (array &optional (trace NIL))
  ((lambda (car-ar cdr-ar)
     (cond
       ((null array) NIL)
       ((check-for-reps car-ar trace) (cons car-ar (del-first-occur cdr-ar trace)))
       (t (del-first-occur cdr-ar (cons car-ar trace)))))
   (car array) (cdr array)
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



;;задание 24

(defun reverse-cdr (list)
  ((lambda (cdr-list)
     (cond
       ((null cdr-list) NIL)
       (t (cons (car list) (reverse-cdr cdr-list)))))
   (cdr list)
   ))

(defun flat-list-to-hight-list (list)
  (cond ((null (cdr list))  list)
   (t (cons (flat-list-to-hight-list (reverse-cdr list)) (last list))))
  )

(defun hight-list-to-flat-list (list)
  ((lambda (cadar-list caar-list cdr-list)
     (cond
       ((null cadar-list) (cons caar-list cdr-list))
       (t (hight-list-to-flat-list (cons caar-list (cons cadar-list cdr-list))))))
   (cadar list) (caar list) (cdr list)
   )
  )



(terpri)
(princ "Никулин - задание 24")
(terpri)
(princ "Тест 1, n=((((4) 3) 2) 1): ")
(princ (hight-list-to-flat-list '((((4) 3) 2) 1)))
(terpri)
(princ "Тест 2, n=(((3) 2) 1): ")
(princ (hight-list-to-flat-list '(((3) 2) 1)))
(terpri)
(princ "Тест 3, n=(1 2 3 4): ")
(princ (flat-list-to-hight-list '(1 2 3 4)))
(terpri)
(princ "Тест 4, n=(1 2 3): ")
(princ (flat-list-to-hight-list '(1 2 3)))



;;задание 25

(defun *abs (x) (if (< x 0) (- x) x))

;;т.к. не уверен, можно ли использовать встроенный mod
;;пишу свой
(defun *mod (x y)
  ((lambda (abs-x abs-y)
     (cond
       ((= (- abs-x abs-y) 0) 0)
       ((< (- abs-x abs-y) 0) (if (< (x y) 0) (- x) x))
       (t (*mod (- x (if (< x 0) (- abs-y) abs-y)) y)))
     )
   (*abs x) (*abs y)
   )
  )
 
(defun is-even (x)
  (cond
    ((= 0 (*mod x 2)) T)
    (t NIL))
  )

(defun kill-all-even-nums (list)
  ((lambda (car-list cdr-list)
    (if car-list
      (cond
        ((is-even car-list) (kill-all-even-nums cdr-list))
        (t (cons car-list (kill-all-even-nums cdr-list))))
      )
     )
   (car list) (cdr list)
   )
  )


(terpri)
(princ "Никулин - задание 25")
(terpri)
(princ "Тест 1, n=(1 1 1 2 2 2): ")
(princ (kill-all-even-nums '(1 1 1 2 2 2)))
(terpri)
(princ "Тест 2, n=(1 1 1 2 2 2 3 3 3): ")
(princ (kill-all-even-nums '(1 1 1 2 2 2 3 3 3)))
(terpri)
(princ "Тест 3, n=(1 2 1 4 1 8 3 3 10): ")
(princ (kill-all-even-nums '(1 2 1 4 1 8 3 3 10)))



;;задание 30

(defun ВЫЧИСЛИ (expr)
  (cond
    ((null expr) nil)
    ((atom  expr) expr)
    (t (eval (cons (second expr) (cons (ВЫЧИСЛИ (first expr))
                                (list (ВЫЧИСЛИ (third expr))))))))
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



;;задание 34



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



;;задание 39

(defun УДАЛИТЬ (target set)
  ((lambda (f-set cdr-set)
     (cond
       ((null set) NIL)
       ((equal target f-set) (УДАЛИТЬ target cdr-set))
       (t (cons f-set (УДАЛИТЬ target cdr-set)))))
   (first set) (cdr set))
  )

(defun ЕЛЕМЕНТ-В-МНОЖЕСТВЕ (target array)
  (cond
    ((null array) NIL)    
    ((equal target (car array)) T)    
    (t (ЕЛЕМЕНТ-В-МНОЖЕСТВЕ target (cdr array)))
    ))

(defun СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ (set1 set2)
  ((lambda (f-set1 cdr-set1 del-fset1-in-set2)
     (cond
       ((null set1) set2)
       ((ЕЛЕМЕНТ-В-МНОЖЕСТВЕ f-set1 set2)
        (СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ cdr-set1 del-fset1-in-set2))
       (t (cons f-set1 (СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ cdr-set1 del-fset1-in-set2)))))
   (first set1) (cdr set1) (УДАЛИТЬ (first set1) set2))
  )

(СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ '(1 2 3) '(2 3 4))

(terpri)
(princ "Никулин - задание 39")
(terpri)
(princ "Тест 1, set1=(1 2 3), set2=(1 2 3 4): ")
(princ (СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ '(1 2 3) '(1 2 3 4)))
(terpri)
(princ "Тест 2, set1=(1 2 3), set2=(4 3 2 1): ")
(princ (СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ '(1 2 3) '(4 3 2 1)))
(terpri)
(princ "Тест 3, set1=(1 2 3), set2=(1 2 3): ")
(princ (СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ '(1 2 3) '(1 2 3)))
(terpri)
(princ "Тест 4, set1=(1 2 3), set2=(3 2 1): ")
(princ (СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ '(1 2 3) '(3 2 1)))
(terpri)
(princ "Тест 5, set1=(1 2 3), set2=(2 3 4): ")
(princ (СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ '(1 2 3) '(2 3 4)))



;;задание 40
(defun УДАЛИТЬ (target set)
  ((lambda (f-set cdr-set)
     (cond
       ((null set) NIL)
       ((equal target f-set) (УДАЛИТЬ target cdr-set))
       (t (cons f-set (УДАЛИТЬ target cdr-set)))))
   (first set) (cdr set))
  )

(defun ЕЛЕМЕНТ-В-МНОЖЕСТВЕ (target array)
  (cond
    ((null array) NIL)    
    ((equal target (car array)) T)    
    (t (ЕЛЕМЕНТ-В-МНОЖЕСТВЕ target (cdr array)))
    ))

(defun РАЗНОСТЬ (set1 set2)
  ((lambda (f-set1 cdr-set1 del-fset1-in-set2)
     (cond
       ((null set1) NIL)
       ((ЕЛЕМЕНТ-В-МНОЖЕСТВЕ f-set1 set2)
        (РАЗНОСТЬ cdr-set1 del-fset1-in-set2))
       (t (cons f-set1 (РАЗНОСТЬ cdr-set1 del-fset1-in-set2)))))
   (first set1) (cdr set1) (УДАЛИТЬ (first set1) set2))
  )

(РАЗНОСТЬ '(1 2 3) '(2 3 4))

(terpri)
(princ "Никулин - задание 40")
(terpri)
(princ "Тест 1, set1=(1 2 3), set2=(1 2 3 4): ")
(princ (РАЗНОСТЬ '(1 2 3) '(1 2 3 4)))
(terpri)
(princ "Тест 2, set1=(1 2 3), set2=(4 3 2 1): ")
(princ (РАЗНОСТЬ '(1 2 3) '(4 3 2 1)))
(terpri)
(princ "Тест 4, set1=(1 2 3 4 5), set2=(2 3 4): ")
(princ (РАЗНОСТЬ '(1 2 3 4 5) '(2 3 4)))
(terpri)
(princ "Тест 5, set1=(1 2 3), set2=(2 3 4): ")
(princ (РАЗНОСТЬ '(1 2 3) '(2 3 4)))



;;задание 47

(defun УДАЛИТЬ-ВСЕ-СВОЙСТВА (target)
  (setf (symbol-plist target) nil))


(setf (get 'Ель 'род) "Хвойные")
(setf (get 'Ель 'Семейство) "Сосновые")
(setf (get 'Ель 'цвет) "Зелёный")

(setf (get 'машина 'цвет) "красный")
(setf (get 'машина 'вес) "большой")
(setf (get 'машина 'марка) "БЕЛАЗ")
(setf (get 'машина 'цена) "5 рублей")

(terpri)
(princ "Никулин - задание 47")
(terpri)
(princ "Тест 1, symbol=Ель, свойста=")
(princ (symbol-plist 'Ель))
(terpri)
(УДАЛИТЬ-ВСЕ-СВОЙСТВА 'Ель)
(princ "Тест 1 после вызова функции, symbol=Ель, свойста=")
(princ (symbol-plist 'Ель))
(terpri)
(princ "Тест 2, symbol=машина, свойста=")
(princ (symbol-plist 'машина))
(terpri)
(УДАЛИТЬ-ВСЕ-СВОЙСТВА 'машина)
(princ "Тест 2 после вызова функции, symbol=машина, свойста=")
(princ (symbol-plist 'машина))


