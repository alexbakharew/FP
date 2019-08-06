;gnu clisp 2.49

(defun words-compare (word1 word2)
    (let ((result NIL)
          (frst-char-word1 word1 0)
          (frst-char-word2 word2 :0)
          (lst-char-word1 word1 (-(length word1) 1))
          (lst-char-word2 word2 (-(length word2) 1))
        )
        (if (and (= frst-char-word1 frst-char-word2) (= lst-char-word1 lst-char-word2)) (setf result T))

        (if (and (= frst-char-word1 lst-char-word2 word2) (= frst-char-word2 word2 lst-char-word1 word1)) (setf result T))
         
    result))

(defun ch(txt)
  (let ((found 0))
    (loop for word1 in txt
      do 
          (loop for word2 in txt
          do 
              (if (words-compare word1 word2) (setf found (+ found 1)))
    ))
    
   (* found 1/2)))
    
                    
                    
(print 
(ch '("а роза упала"
      "на лапу азора") ))