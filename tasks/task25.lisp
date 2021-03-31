(defun *abs* (x) (if (< x 0) (- x) x))

;;т.к. не уверен, можно ли использовать встроенный mod
;;пишу свой
(defun *mod* (x y)
  ((lambda (abs-x abs-y)
     (cond
       ((= (- abs-x abs-y) 0) 0)
       ((< (- abs-x abs-y) 0) (if (< (* x y) 0) (- x) x))
       (t (*mod* (- x (if (< x 0) (- abs-y) abs-y)) y)))
     )
   (*abs* x) (*abs* y)
   )
  )
 
(defun is-even (x)
  (cond
    ((= 0 (*mod* x 2)) T)
    (t NIL))
  )

(defun kill-all-even-nums (*list*)
  ((lambda (car-list cdr-list)
    (if car-list
      (cond
        ((is-even car-list) (kill-all-even-nums cdr-list))
        (t (cons car-list (kill-all-even-nums cdr-list))))
      )
     )
   (car *list*) (cdr *list*)
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
