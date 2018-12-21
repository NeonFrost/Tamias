(define-state rotated-lines)
(setf state rotated-lines)
#|
(defun draw-lines ()
  (let ((x1 240)
	(y1 180)
	(dirs (list 2 2))
	(x-dir 79)
	(y-dir 0))
    (loop for a below 360
       do (draw-line x1 y1 (1+ (+ (- x1 x-dir) y1)) (1+ (+ (- y1 y-dir) x1)))
	 (incf x-dir (car dirs))
	 (incf y-dir (cadr dirs))
	 (incf x1 1)
	 (incf y1 1)
	 (if (> x-dir 79)
	     (setf (car dirs) -2)
	     (if (< x-dir 1)
		 (setf (car dirs) 2)))
	 (if (> y-dir 79)
	     (setf (cadr dirs) -2)
	     (if (< y-dir 1)
		 (setf (cadr dirs) 2)))
	 )))|#
(princ (+ 8 4 4 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ))
(defun draw-lines ()
  (let ((x1 300)
	(y1 300)
	(x2 300)
	(y2 225)
	(to-change-list (list 8 4 2 2 4 2 2 2 2 2 1 1 2 1 1 1 1 1 1 1 1 ))
	(tmp-change-list nil)
	(cur-dir 'r-d)
	(tmp-acc 0)
	(change-list nil)
	(dirs (list 2 0)))
    #|
    Every 45 degrees, it changes what the focus will be
    0-45 focused on going to the right, continuing downward
    45-90 focused on going down, continuing right
    90-135 focused on going down, going left
    135-180 focused on going left, going down
    180-225 focused on going left, going up
    225-270 focused on going up, going left
    270-315 focused on going up, going right
    315-360 focused on going right, going up
    |#
    (loop for a below 360
       do (if (eq tmp-acc (car to-change-list))
	      (progn (case cur-dir
		       (r-d (push (list 2 1) change-list))
		       (d-r (push (list 1 2) change-list))
		       (d-l (push (list -1 2) change-list))
		       (l-d (push (list -2 1) change-list))
		       (l-u (push (list -2 -1) change-list))
		       (u-l (push (list -1 -2) change-list))
		       (u-r (push (list 1 -2) change-list))
		       (r-u (push (list 2 -1) change-list)))
		     (push (car to-change-list) tmp-change-list)
		     (setf to-change-list (cdr to-change-list)
			   tmp-acc 0))
	      (push dirs change-list))
	 (if (not to-change-list)
	     (progn (setf to-change-list tmp-change-list
			  tmp-change-list nil)
		    (case cur-dir
		      (r-d (setf cur-dir 'd-r
				 dirs (list 0 2)))
		      (d-r (setf cur-dir 'd-l
				 dirs (list -0 2)))
		      (d-l (setf cur-dir 'l-d
				 dirs (list -2 0)))
		      (l-d (setf cur-dir 'l-u
				 dirs (list -2 -0)))
		      (l-u (setf cur-dir 'u-l
				 dirs (list -0 -2)))
		      (u-l (setf cur-dir 'u-r
				 dirs (list 0 -2)))
		      (u-r (setf cur-dir 'r-u
				 dirs (list 2 -0)))
		      (r-u (setf cur-dir 'r-d
				 dirs (list 2 0))))))
	 (incf tmp-acc))
    (setf change-list (reverse change-list))
    (loop for dir in change-list
       do (draw-line x1 y1 x2 y2)
	 (incf x2 (car dir))
	 (incf y2 (cadr dir))
	 )))
(add-to-state-render draw-lines rotated-lines)
    
