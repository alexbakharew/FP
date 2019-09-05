;gnu clisp 2.49

;gnu clisp 2.49

(defclass cart ()                ; имя класса и надклассы
 ((x :initarg :x :reader cart-x)   ; дескриптор слота x
  (y :initarg :y :reader cart-y))) ; дескриптор слота y

(defclass polar ()
 ((radius :initarg :radius :accessor radius) 	; длина >=0
  (angle  :initarg :angle  :accessor angle)))	; угол (-π;π]

(defclass triangle ()
 ((vertex1 :initarg :1 :reader vertex1)  ; селектор может совпадать
  (vertex2 :initarg :2 :reader vertex2)  ; с именем слота
  (vertex3 :initarg :3 :reader vertex3)))

(defmethod print-object ((c cart) stream)
  (format stream "[CART x ~d y ~d]"(cart-x c) (cart-y c) ))

(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))

(defmethod print-object ((tri triangle) stream)
  (format stream "[ТРЕУГ ~s ~s ~s]"
          (vertex1 tri) (vertex2 tri) (vertex3 tri)))

(defmethod cart-x ((p polar))
  (* (radius p) (cos (angle p))))

(defmethod cart-y ((p polar))
  (* (radius p) (sin (angle p))))

(defgeneric to-cart (arg)
 (:documentation "Преобразование аргумента в декартову систему.")
 (:method ((c cart))
  c)
 (:method ((p polar))
  (make-instance 'cart
                 :x (cart-x p)
                 :y (cart-y p))) )

(defun square (x) (* x x))

(defun good-enough-p (guess x)
  (<= (abs (- (square guess) x)) 0.001))

(defun average (x y)
  (/ (+ x y) 2))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-iter (guess x)
  (if (good-enough-p guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun sqrt2 (x)
 (sqrt-iter 1.0 x))

(defun pow2 (x) (* x x))

(defmethod calc-dist ((c1 cart) (c2 cart))
    (sqrt2 (+ (pow2 (abs (- (cart-x c1) (cart-x c2))) ) (pow2 (abs (- (cart-y c1) (cart-y c2))))))
    )

(defmethod get-len ((p1 polar) (p2 polar))
    (setq dot1 (to-cart p1))
    (setq dot2 (to-cart p2))
    (calc-dist dot1 dot2)
)    

(defmethod get-len ((c1 cart) (p2 polar))
    (setq dot2 (to-cart p2))
    (calc-dist c1 dot2)
) 

(defmethod get-len ((p1 polar) (c2 cart))
    (setq dot1 (to-cart p1))
    (calc-dist dot1 c2)
) 

(defmethod get-len ((c1 cart) (c2 cart))
    (calc-dist c1 c2)
) 

(defun прямоугольный-p (tri)
  (setq epsilon 0.0001)
  (setq a (get-len (vertex1 tri) (vertex2 tri)))
  (setq b (get-len (vertex2 tri) (vertex3 tri)))
  (setq c (get-len (vertex3 tri) (vertex1 tri)))
  
  (print a)
  (print b)
  (print c)
    
  (print (square a))
  (print (square b))
  (print (square c))
    
   (cond   ((< (- (square a) (+ (square b) (square c))) epsilon)  T)
           ((< (- (square b) (+ (square c) (square a))) epsilon)  T)
           ((< (- (square c) (+ (square a) (square b))) epsilon)  T)
       
   )
    )

(setq p1 (make-instance 'polar :radius 5 :angle (/ pi 6)))
(setq p2 (make-instance 'polar :radius 2 :angle (/ pi 3)))
(setq c1 (make-instance 'cart :x 0 :y 0))
(setq c2 (make-instance 'cart :x 1 :y 1))

;(print (get-len p1 p2))
;(print (get-len p1 c1))
;(print (get-len p2 c2))

(setq tri1 (make-instance 'triangle
           :1 (make-instance 'cart :x -2 :y 10)
           :2 (make-instance 'cart :x 7 :y -8)
           :3 (make-instance 'polar :radius 6 :angle (/ pi -4))))

(setq tri (make-instance 'triangle
           :1 (make-instance 'cart :x 0 :y 0)
           :2 (make-instance 'cart :x 3 :y 0)
           :3 (make-instance 'cart :x 3 :y 4)))
;(print-object tri *standard-output*)
(print (прямоугольный-p tri1))
;;(print-object p *standard-output*)
;;(print-object c *standard-output*)  



       
