(defmacro read-file (file)
  `(with-temp-buffer
     (insert-file-contents ,file)
     (buffer-string)))

;; Taken from https://emacs.stackexchange.com/questions/29786/how-to-remove-delete-nth-element-of-a-list
(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list) ;; If nth is zero, just return cdr of list
    (let ((last (nthcdr (1- nth) list))) ;; Get cdr of object before nth
      (setcdr last (cddr last)) ;; Set the cdr of last to the cdr of the cdr of last (i.e. the object after nth)
      list))) 

(defun process-input (file)
  (let ((input (butlast (split-string (read-file file) "\n")))
	(reports '()))
    (dolist (report input)
      (push (mapcar 'string-to-number (split-string report " ")) reports))
    reports))

;; Wanted to use recursion, but Elisp doesn't support tail call optimisation!
;; This wouldn't be an issue for this challenge, but I want to learn idiomatic Elisp.
(defun check-safe (report)
  (let* ((previous (car report))
	 (report (cdr report))
	 (direction (cond
		     ((< (car report) previous)
		      '<)
		     ((> (car report) previous)
		      '>)
		     (t
		      nil)))
	 (safe (if direction
		   t
		 nil))
	 (i 0))
    (cl-dolist (level report safe)
      (setq i (1+ i))
      (if (and
	   safe
	   (funcall direction level previous)
	   (<= (abs (- previous level)) 3))
	  (setq previous level)
	(cl-return nil)))))

;; Part 1
(let ((reports (process-input "input.txt"))
      (total-safe 0))
  (dolist (report reports total-safe)
    (if (check-safe report) (setq total-safe (1+ total-safe)))))

;; Part 2
(let ((reports (process-input "input.txt"))
      (total-safe 0))
  (dolist (report reports total-safe)
    (let ((nth 0)
	  (safe (check-safe report))
	  (length (length report)))
      (while (and (not safe) (< nth length))
	(setq safe (check-safe (remove-nth-element nth (cl-copy-list report)))
	      nth (1+ nth)))
      (if safe
	  (setq total-safe (1+ total-safe))))))

