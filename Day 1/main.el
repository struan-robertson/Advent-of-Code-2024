(load-file "../lib.el")

(defun process-input (file)
  (let ((input (butlast (split-string (read-file file) "\n")))
	(left ()) 
	(right ()))
    (dolist (line input)
      (let ((line-split (split-string line "   ")))
	(push (string-to-number (car line-split)) left)
	(push (string-to-number (cadr line-split)) right)))
    (list (sort left '<) (sort right '<))))

(defun calculate-difference (ids)
  (let ((left (car ids))
	(right (cadr ids)))
    (cl-reduce '+ (cl-mapcar (lambda (x y) (abs (- x y))) left right))))

(defun calculate-similarity (ids)
  (let ((left (car ids))
	(right (cadr ids)))
    (cl-reduce '+ (mapcar (lambda (x) (* x (cl-count x right))) left))))

;; Part 1
(calculate-difference (process-input "input"))

;; Part 2
(calculate-similarity (process-input "input"))
