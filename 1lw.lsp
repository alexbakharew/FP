(defun check-condition (c a b); c is the longest side of the triangle
    (< (+ (* a a) (* b b) ) (* c c)) 
)
        
(defun obtuse-angled (a b c)
    (if (or (>= a (+ b c)) (>= b (+ a c)) (>= c (+ a b))) Nil
    
        (let ((max_side (max a b c)))

            (cond   ((= max_side a) (check-condition a b c))
                    ((= max_side b) (check-condition b a c))
                    ((= max_side c) (check-condition c a b))
            )
        )
    )
)
; testing
(print (obtuse-angled 7.0 3.3 4.2))
(print (obtuse-angled 5.0 3.3 4.2))
(print (obtuse-angled 3 4 5))
(print (obtuse-angled 3 11 10))
(print (obtuse-angled 1 2 4))
(print (obtuse-angled 4 4 4))
