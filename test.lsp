;gnu clisp 2.49

(defclass cart ()                ; имя класса и надклассы
 ((x :initarg :x :reader cart-x)   ; дескриптор слота x
  (y :initarg :y :reader cart-y))) ; дескриптор слота y

;; Метод для удобной распечатки экземпляра
(defmethod print-object ((c cart) stream)
  (format stream "[CART x ~d y ~d]"

(setq c (make-instance 'cart :x 4 :y 3))

;; (slot-value c 'x) => 4
;; (slot-value c 'y) => 3

;; (cart-x c) => 4
;; (cart-y c) => 3


(defclass polar ()
 ((radius :initarg :radius :accessor radius) 	; длина >=0
  (angle  :initarg :angle  :accessor angle)))	; угол (-π;π]

(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))

(setq p (make-instance 'polar :radius 5 :angle (/ pi 6)))
;;[POLAR radius 5 angle 0.5235988]

;;(radius p) => 5
;;(angle p)  => 0.5235988

(setf (slot-value p 'radius) 10) ; присваивание по имени
(setf (angle p) (/ pi 4))    

(defclass triangle ()
 ((vertex1 :initarg :1 :reader vertex1)  ; селектор может совпадать
  (vertex2 :initarg :2 :reader vertex2)  ; с именем слота
  (vertex3 :initarg :3 :reader vertex3)))

(defmethod print-object ((tri triangle) stream)
  (format stream "[ТРЕУГ ~s ~s ~s]"
          (vertex1 tri) (vertex2 tri) (vertex3 tri)))

(setq tri (make-instance 'triangle
           :1 (make-instance 'cart :x 4 :y 3)
           :2 (make-instance 'cart :x 7 :y 5)
           :3 (make-instance 'cart :x 5 :y -1)))