;gnu clisp 2.49

(defclass cart ()                ; имя класса и надклассы
 ((x :initarg :x :reader cart-x)   ; дескриптор слота x
  (y :initarg :y :reader cart-y))) ; дескриптор слота y

(defclass polar ()
 ((radius :initarg :radius :accessor radius) 	; длина >=0
  (angle  :initarg :angle  :accessor angle)))	; угол (-π;π]

(defmethod print-object ((c cart) stream)
  (format stream "[CART x ~d y ~d]"))
          
(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))

(setq p (make-instance 'polar :radius 5 :angle (/ pi 6)))
(setq c (make-instance 'cart :x 4 :y 3))

;;(print-object p )
p