(defun reverse-cdr (*list*)
  ((lambda (cdr-list)
     (cond
       ((null cdr-list) NIL)
       (t (cons (car *list*) (reverse-cdr cdr-list)))))
   (cdr *list*)
   ))

(defun flat-list-to-hight-list (*list*)
  (cond ((null (cdr *list*))  *list*)
   (t (cons (flat-list-to-hight-list (reverse-cdr *list*)) (last *list*))))
  )

(defun hight-list-to-flat-list (*list*)
  ((lambda (cadar-list caar-list cdr-list)
     (cond
       ((null cadar-list) (cons caar-list cdr-list))
       (t (hight-list-to-flat-list (cons caar-list (cons cadar-list cdr-list))))))
   (cadar *list*) (caar *list*) (cdr *list*)
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
