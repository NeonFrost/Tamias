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

(define-state draw-cirlce-state)

(defvar arc-path '(4 3 2 1))
(defvar dirs '(r-d d-r d-l l-d l-u u-l u-r r-u))
(defun render-pixel (x y &key (color '(255 255 255 255)))
  (render-box x y 1 1 :color color))
(defun draw-circle ()
	   (let ((x 200)
		 (y 200)
		 (path-list arc-path))
	     (loop for cur-dir below (length dirs)
		do (loop for path in path-list
		      do (loop for n below path
			    do (case (nth cur-dir dirs)
				 (r-d (incf x 1))
				 (d-r (incf y 1))
				 (d-l (incf y 1))
				 (l-d (decf x 1))
				 (l-u (decf x 1))
				 (u-l (decf y 1))
				 (u-r (decf y 1))
				 (r-u (incf x 1)))
			      (render-pixel x y))
			(if (< (position path path-list) (1- (length path-list)))
			    (case (nth cur-dir dirs)
			      (r-d (incf y 1))
			      (d-r (incf x 1))
			      (d-l (decf x 1))
			      (l-d (incf y 1))
			      (l-u (decf y 1))
			      (u-l (decf x 1))
			      (u-r (incf x 1))
			      (r-u (decf y 1)))))
		  (case (nth cur-dir dirs)
		    (r-d (incf x 1))
		    (d-r (incf y 1)
			 (render-pixel x y))
		    (d-l (incf y 1))
		    (l-d (decf x 1)
			 (render-pixel x y))
		    (l-u (decf x 1))
		    (u-l (decf y 1)
			 (render-pixel x y))
		    (u-r (decf y 1))
		    (r-u (incf x 1)
			 (render-pixel x y)))
		  (setf path-list (reverse path-list)))))
(add-to-state-render draw-circle draw-cirlce-state)

#|

Another methodology for drawing a curved arc:
First get the slope between two points in a rectangle
We'll call these two points, which are the beginning and end of an arc, A and B
After getting the slope of A and B, determine the midpoint, which will be called C
Create a new point D that is a straight line from C, so as to be perpendicular to line AB
Determine the midpoint of CD, E
Determine slope of AE, where the Angle of CAD is 45 degrees
Divide the slope of X of AE by the y slope of AE
This is the amount of X steps required to create a curved arc in the X direction
Upon completion of the X steps, switch to Y steps
Determine an evenly spaced out step amount, where it is something like (4 2 1)
Move 4 in the x direction, rendering a pixel per point
Move 1 in the y direction, render
Move 2 in X, render
Move 1 in y
Move 1 in X
Reverse the list, and switch to Y steps
upon completion of the X and Y steps, a curved line will be drawn

Drawing a circle:
Arc method, but upon completion of Arc 1, reverse X direction
Arc 2, reverse y
Arc 3, reverse X
Arc 4 will be the completed circle

Example:
        A----------------------------D
         \--\  	         	 --/
	  \  -----\   	      --/
	   \	   ---\     -/
            \	       ---E/
	     \	      --/|
	      \	   --/	 |
	       \C-/   	/
                \     	|
		 \    	|
		  \    /
		   \   |
		    \ /
		     \|
                      B
|#
