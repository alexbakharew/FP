;gnu clisp 2.49

(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline)))

(defun word-list (string)
  ;; Разбить строки на слова, разделённые знаками whitespace
  ;; A la (split-seq-if #'whitespace-char-p string)
  (loop with len = (length string)
        for left = 0 then (1+ right)
        for right = (or (position-if #'whitespace-char-p string
                                     :start left)
                        len)
        unless (= right left)	; исключить пустые слова
          collect (subseq string left right)
        while (< right len)))
;;--------------------------------------------------------------
(defun russian-upper-case-p (char)
  (position char "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))

(defun russian-char-downcase (char)
  (let ((i (russian-upper-case-p char)))
    (if i 
        (char "абвгдеёжзийклмнопрстуфхцчшщъыьэюя" i)
        (char-downcase char))))

(defun russian-char-equal (char1 char2)
  (char-equal (russian-char-downcase char1)
              (russian-char-downcase char2)))
;;--------------------------------------------------------------
(defun english-upper-case-p (char)
  (position char "ABCDEFGHIGKLMNOPQRSTUVWXYZ"))

(defun english-char-downcase (char)
  (let ((i (english-upper-case-p char)))
    (if i 
        (char "abcdefghigklmnopqrstuvwxyz" i)
        (char-downcase char))))

(defun english-char-equal (char1 char2)
  (char-equal (english-char-downcase char1)
              (english-char-downcase char2)))
;;--------------------------------------------------------------
(defun count-words-with-start-eq-end(txt)
    (let ((found 0))
  (dolist (sentence txt)
      (dolist (word (word-list sentence))
        
       (let ((first-char NIL) (last-char NIL))
            (setf first-char (char word 0))
            (setf last-char (char word (- (length word) 1)))
            
            (setf rus-res(russian-char-equal first-char last-char))
            (setf eng-res(english-char-equal first-char last-char))
            (if (and rus-res eng-res) (setf found (+ found 1)))
        )
    ))
         found)
    )
     
(print (count-words-with-start-eq-end 
        '("а роза упала"
      "на лапу азора") ))

(print (count-words-with-start-eq-end 
        '("cat dog frog bob"
      "на лапу азора") ))

(print (count-words-with-start-eq-end 
        '("cat dog bat Bob"
      "кот тоТ Боб") ))

(print (count-words-with-start-eq-end 
        '("cat dog bat nob") ))