;; (defun foo (val) (* arg 2))
;; sbcl --script test.lsp
;; first lab
;; (defun foo (val) 

;;     (< val 10)


;; )
;; ;; (print (foo 2))
;; (defun obtuse-angled (a b c)
;;     (+ a b c)
;; )
;; (print (obtuse-angled 7.0 2.0 5.0))
;; end of first lab

;; (defun print-list (l)
;;     (print l)
;; )
(defvar l (list 1 3 4 5 6))
;; (print l)
;; (print-list (list 1 2 3 4))
(defun print-list (l i)
    (print (nth i l))
)
;; (print-list (list 1 2 3 4 5) 3)

(defun nth-element (i l)
    (cond   ((= i 0) (first l)) 
            ((null l) Nil)
            ((nth-element (1- i) (rest l)))
    )
    
    
    ;; (if (= i 0) (first l))
    ;; (if (= (null x) T) Nil)
    ;; (nth-element (- i 1) (rest l))
)

(print (nth-element 10 (list 1 2 3 4 56)))
