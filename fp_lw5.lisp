

(defgeneric sub2 (arg1 arg2)
 (:method ((n1 number) (n2 number))
  (- n1 n2)))

(defun make-term (&key order coeff)
  (list order coeff))

(defun order (term) (first term))  
(defun coeff (term) (second term))


(defclass polynom ()
 ((var-symbol :initarg :var :reader var)
  ;; Разреженный список термов в порядке убывания степени
  (term-list :initarg :terms :reader terms)))

(defgeneric zerop1 (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

(defgeneric minusp1 (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

(defmethod print-object ((p polynom) stream)
  (format stream "[МЧ (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
          (var p)
          (mapcar (lambda (term)
                    (list (zerop1 (coeff term))
                          (minusp1 (coeff term))
                          (if (minusp1 (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (var p)
                          (order term)))
                  (terms p))))




(defun make-polynom(n)
    (let ((p 0))
         (setq p (make-instance 'polynom 
          :var 'x
          :terms (list (make-term :order 0 :coeff n))))
         p)
    )
    

(setq p1 (make-instance 'polynom ;полином 1
          :var 'x
          :terms (list (make-term :order 2 :coeff 5)
                       (make-term :order 1 :coeff 3.3)
                       (make-term :order 0 :coeff -7))))


(defmethod sub2 ((p1 polynom) n)
   (setq p2 (make-polynom n))
  (if (same-variable-p (var p1) (var p2))
      (make-instance 'polynom
                     :var (var p1)
                     :terms (add-terms (terms p1)
                                       (terms p2)))
      (error "Многочлены от разных переменных: ~s и ~s"
             p1 p2)))

(defun same-variable-p (v1 v2)
  ;; Переменные v1 и v2 - совпадающие символы
  (and (symbolp v1) (symbolp v2) (eq v1 v2)))

(defun add-terms (tl1 tl2)
  ;; Объединить списки термов tl1 и tl2,
  ;; упорядочивая по убыванию степеней
  (cond ((null tl1) tl2)
        ((null tl2) tl1)
        (t
         (let ((t1 (first tl1))
               (t2 (first tl2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1
                               (add-terms (rest tl1) tl2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2
                               (add-terms tl1 (rest tl2))))
                 (t
                  (adjoin-term  
                   (make-term :coeff (sub2 (coeff t1) (coeff t2))
                              :order (order t1))
                   (add-terms (rest tl1)
                              (rest tl2)))))))))

(defun adjoin-term (term term-list)
  ;; Добавить term к списку term-list
  (if (zerop1 (coeff term))   ; если коэффициент нулевой,
      term-list               ; то отбрасываем терм,
      (cons term term-list))) ; иначе накапливаем



(print (sub2 p1 9))
