(setf  A (make-array '(3 3) :initial-contents '((1 2 5) (3 4 5) (3 3 3))))
(setf  B (make-array '(3 3) :initial-contents '((-11 -12 -50) (-13 -14 5) (3 3 -60))))


(defun print-matrix (matrix &optional (chars 3) stream)
  (let ((*print-right-margin* (+ 6 (* (1+ chars)
                                      (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))

(defun min-elem(matrix)
    (let ((min_val (aref matrix 0 0))
          (N (array-dimension matrix 0))
          (M (array-dimension matrix 1))
          (result (make-array '(N M)))
          )
         
        (dotimes (i (array-dimension matrix 0))
            (dotimes (j (array-dimension matrix 1))
                (if ( < (aref matrix i j) min_val) (setf min_val (aref matrix i j)))
            ) 
        )
             
         
         
         
         
    min_val
         
         
         
    )
    
)


(print (min-elem B))
