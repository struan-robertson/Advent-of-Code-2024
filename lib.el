(defmacro read-file (file)
  "Read file contents into string."
  `(with-temp-buffer
     (insert-file-contents ,file)
     (buffer-string)))

(defun re-seq (regexp string &optional indices)
  "Return regex matches, and optionally the index of the
   first char of each match."
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

;; Taken from https://emacs.stackexchange.com/questions/29786/how-to-remove-delete-nth-element-of-a-list
(defun remove-nth-element (nth list)
  "Delete nth element from list."
  (if (zerop nth) (cdr list) ;; If nth is zero, just return cdr of list
    (let ((last (nthcdr (1- nth) list))) ;; Get cdr of object before nth
      (setcdr last (cddr last)) ;; Set the cdr of last to the cdr of the cdr of last (i.e. the object after nth)
      list))) 

(defmacro ndidx (sequence &rest idxs)
  "Return item at specified indices from n dimensional sequence"
  (let ((forms `(elt ,sequence ,(pop idxs))))
    (dolist (idx idxs forms)
      (setq forms `(elt ,forms ,idx)))))
