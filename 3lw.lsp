(setf  A (make-array '(3 3) :initial-contents '((1 2 5) (3 4 1) (3 1 1))))
(setf  B (make-array '(3 3) :initial-contents '((-11 -60 -50) (-60 -14 5) (3 3 -60))))


(defun print-matrix (matrix &optional (chars 3) stream)
  (let ((*print-right-margin* (+ 6 (* (1+ chars)
                                      (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))

(defun min-elem(matrix val)
    (let ((min_val (aref matrix 0 0))
          (result (make-array (array-dimensions A)))
          
          )
         
        (dotimes (i (array-dimension matrix 0))
            (dotimes (j (array-dimension matrix 1))
                (if ( < (aref matrix i j) min_val) (setf min_val (aref matrix i j)))
            ) 
        )
             
    ;min_val
        (dotimes (i (array-dimension matrix 0))
            (dotimes (j (array-dimension matrix 1))
                (if ( = (aref matrix i j) min_val) (setf (aref result i j) val)
                    (setf (aref result i j) (aref matrix i j))
                    )
            ) 
        )
         
      result
    )
    
)
(print-matrix (min-elem A 5.5))
(print-matrix (min-elem B 5.5))
(print "--")
(print-matrix A)
(print-matrix B)
