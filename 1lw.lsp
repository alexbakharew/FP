(defun check-condition (c a b); c is the longest side of the triangle
    (if (< (+ (* a a) (* b b) ) (* c c)) T
        NIL))
(defun obtuse-angled (a b c)
    (cond   ((= (max a b c) a) (check-condition a b c))
            ((= (max a b c) b) (check-condition b a c))
            ((= (max a b c) c) (check-condition c a b))
    )
)
; testing
(print (obtuse-angled 7.0 3.3 4.2))
(print (obtuse-angled 5.0 3.3 4.2))
(print (obtuse-angled 3 4 5))
(print (obtuse-angled 3 11 10))
