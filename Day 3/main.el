(defmacro read-file (file)
  `(with-temp-buffer
     (insert-file-contents ,file)
     (buffer-string)))

(defun re-seq (regexp string &optional indices)
  (save-match-data 
    (let ((pos 0) 
	  (matches '())
	  (positions '())) 
      (while (string-match regexp string pos) 
	(push (match-string 0 string) matches) 
	(setq pos (match-end 0))
	(push (match-beginning 0) positions))
      (if indices
	  (cl-mapcar (lambda (x y) `(,x . ,y)) matches positions)
	matches))))

(defun parse-mul (instruction)
  (let ((numbers (mapcar 'string-to-number (re-seq "[0-9]+" instruction))))
    (cl-reduce '* numbers)))

;; Part 1
(let ((instructions (re-seq "mul([0-9]+,[0-9]+)" (read-file "input"))))
  (cl-reduce '+ (mapcar 'parse-mul instructions)))

;; Part 2
(let* ((input (read-file "input"))
       (mul (re-seq "mul([0-9]+,[0-9]+)" input t))
       (do (re-seq "do()" input t))
       (dont (re-seq "don't()" input t))
       (instructions (sort
		      (append mul do dont)
		      (lambda (x y)
			(< (cdr x) (cdr y)))))
       (enabled t)
       (result 0))
  (dolist (instruction (mapcar (lambda (x) (car x)) instructions) result)
    (cond
     ((and enabled (string-match-p "mul" instruction))
      (cl-incf result (parse-mul instruction)))
     ((string= "do()" instruction)
      (setq enabled t))
     ((string= "don't()" instruction)
      (setq enabled nil)))))

