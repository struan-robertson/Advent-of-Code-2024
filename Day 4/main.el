(load-file "../lib.el")

(defun process-input (file)
  (let ((input (vconcat (butlast (split-string (read-file file) "\n")))))
    input))

(process-input "input")

(defmacro mas (c1 c2 c3)
  `(if (and
	(char-equal ,c1 ?m)
	(char-equal ,c2 ?a)
	(char-equal ,c3 ?s))
       t
     nil))

(defmacro unordered-mas (c1 c2 c3)
  `(or (mas ,c1 ,c2 ,c3) (mas ,c3 ,c2 ,c1)))

;; Part 1
(let* ((input (process-input "input"))
       (total 0)
       (y 0)
       (x 0)
       (y-length (length input))
       (x-length (length (ndidx input 0))))
  (while (< y y-length)
    (while (< x x-length)
      (if (char-equal (ndidx input y x) ?x)
	  (progn
	    ;; Up
	    (if (and (>= y 3)
		     (mas
		      (ndidx input (- y 1) x)
		      (ndidx input (- y 2) x)
		      (ndidx input (- y 3) x)))
		(setq total (1+ total)))

	    ;; Down
	    (if (and (<= y (- y-length 4))
		     (mas
		      (ndidx input (+ y 1) x)
		      (ndidx input (+ y 2) x)
		      (ndidx input (+ y 3) x)))
		(setq total (1+ total)))

	    ;; Left
	    (if (and (>= x 3)
		     (mas
		      (ndidx input y (- x 1))
		      (ndidx input y (- x 2))
		      (ndidx input y (- x 3))))
		(setq total (1+ total)))
	    
	    ;; Right
	    (if (and (<= x (- x-length 4))
		     (mas
		      (ndidx input y (+ x 1))
		      (ndidx input y (+ x 2))
		      (ndidx input y (+ x 3))))
		(setq total (1+ total)))

	    ;; Up-Left Diagonal
	    (if (and (>= y 3)
		     (>= x 3)
		     (mas
		      (ndidx input (- y 1) (- x 1))
		      (ndidx input (- y 2) (- x 2))
		      (ndidx input (- y 3) (- x 3))))
		(setq total (1+ total)))

	    ;; Up-Right Diagonal
	    (if (and (>= y 3)
		     (<= x (- x-length 4))
		     (mas
		      (ndidx input (- y 1) (+ x 1))
		      (ndidx input (- y 2) (+ x 2))
		      (ndidx input (- y 3) (+ x 3))))
		(setq total (1+ total)))

	    ;; Down-Left Diagonal
	    (if (and (<= y (- y-length 3))
		     (>= x 3)
		     (mas
		      (ndidx input (+ y 1) (- x 1))
		      (ndidx input (+ y 2) (- x 2))
		      (ndidx input (+ y 3) (- x 3))))
		(setq total (1+ total)))

	    ;; Down-Right Diagonal
	    (if (and (<= y (- y-length 4))
		     (<= x (- x-length 4))
		     (mas
		      (ndidx input (+ y 1) (+ x 1))
		      (ndidx input (+ y 2) (+ x 2))
		      (ndidx input (+ y 3) (+ x 3))))
		(setq total (1+ total)))))
      (setq x (1+ x)))
    (setq y (1+ y)
	  x 0))
  total)


;; Part 2
(defun runit ()
  (let* ((input (process-input "input"))
	 (total 0)
	 (crossed 0)
	 (y 0)
	 (x 0)
	 (y-length (length input))
	 (x-length (length (ndidx input 0))))
    (while (< y y-length)
      (while (< x x-length)
	(if (char-equal (ndidx input y x) ?a)
	    (progn
	      ;; Down-Diagonal
	      (if (and (>= y 1) (<= y (- y-length 2))
		       (>= x 1) (<= x (- x-length 2))
		       (unordered-mas
			(ndidx input (- y 1) (- x 1))
			(ndidx input y x)
			(ndidx input (+ y 1) (+ x 1))))
		  (setq crossed (1+ crossed)))

	      ;; Up-Diagonal
	      (if (and (>= y 1) (<= y (- y-length 2))
		       (>= x 1) (<= x (- x-length 2))
		       (unordered-mas
			(ndidx input (- y 1) (+ x 1))
			(ndidx input y x)
			(ndidx input (+ y 1) (- x 1))))
		  (setq crossed (1+ crossed)))
	      
	      (setq total (+ total (if (= crossed 2) 1 0))
		    crossed 0)
	      ))
	(setq x (1+ x)))
      (setq y (1+ y)
	    x 0))
    total))
