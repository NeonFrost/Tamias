(define-state snake)
(setf state snake)
(defvar game-board '(60 60))
(defstruct segment
  (x 0)
  (y 0))
(setf *screen-width* (* (car game-board) 10))
(setf *screen-height* (* (car game-board) 10))
(defvar segment-offset (list (round (/ *screen-width* (car game-board)))
			     (round (/ *screen-height* (cadr game-board)))))
(defstruct snake
  segments
  (length 1)
  (color +pastel-green+))
(defvar key-processing? nil)

(defvar snek (make-snake :segments (list (make-segment :x 15 :y 10))))
(defvar current-direction '(0 0))

;;the current way the macro is currently called, it works by putting the child at the "end" of the tail
;;i.e. nth 0 snek-segs
;;an alternative way is to add it onto the head at * -1 car and cadr cur-dir
;;i.e. push it on the end, aka append
(defun make-child-segment (parent-segment)
  (push (make-segment :x (segment-x parent-segment)
		      :y (segment-y parent-segment))
	(snake-segments snek)))

(defvar segment-location (list 20 10))

(defun snek-collide (obj)
  (let ((head-segment (nth 0 (snake-segments snek))))
    (case obj
      (food (and (eq (segment-x head-segment) (car segment-location))
		 (eq (segment-y head-segment) (cadr segment-location))))
      )))
(defun move ()
  (setf key-processing? nil)
  (if (snek-collide 'food)
      (progn (incf (snake-length snek) 1)
	     (setf segment-location (list (random (car game-board))
					  (random (cadr game-board))))
	     (make-child-segment (car (snake-segments snek))))
      (if current-direction
	  (progn (make-child-segment (car (snake-segments snek)))
		 (incf (segment-x (car (snake-segments snek))) (car current-direction))
		 (incf (segment-y (car (snake-segments snek))) (cadr current-direction))
		 (if (> (length (snake-segments snek)) (snake-length snek))
		     (setf (nth (1- (length (snake-segments snek))) (snake-segments snek)) nil
			   (snake-segments snek) (remove nil (snake-segments snek))))))))
(add-loop-function move snake 'top)

(defun render-game-board ()
  (loop for segment in (snake-segments snek)
     do (let ((snek-color (if (< (position segment (snake-segments snek)) (1- (length (snake-segments snek))))
			      (if (eq (position segment (snake-segments snek)) 0)
				  +natural-green+
				  (snake-color snek))
			      +pastel-yellow+)))
	    (render-box (* (segment-x segment) (car segment-offset))
			(* (segment-y segment) (cadr segment-offset))
			(car segment-offset) (cadr segment-offset)
			:color snek-color)))
  (render-box (* (car segment-location) (car segment-offset))
	      (* (cadr segment-location) (cadr segment-offset))
	      (car segment-offset) (cadr segment-offset)
	      :color +dark-pastel-pink+))
(add-to-state-render render-game-board snake)

(defun set-cur-dir (lst)
  (if (not key-processing?)
      (case (nth 2 current-direction)
	(u (if (not (eq (nth 2 lst) 'd))
	       (setf current-direction lst)))
	(l (if (not (eq (nth 2 lst) 'r))
	       (setf current-direction lst)))
	(r (if (not (eq (nth 2 lst) 'l))
	       (setf current-direction lst)))
	(d (if (not (eq (nth 2 lst) 'u))
	       (setf current-direction lst)))
	(otherwise (setf current-direction lst))))
  (setf key-processing? t))
  
(add-key :scancode-up snake :down (set-cur-dir '(0 -1 u)))
(add-key :scancode-left snake :down (set-cur-dir '(-1 0 l)))
(add-key :scancode-right snake :down (set-cur-dir '(1 0 r)))
(add-key :scancode-down snake :down (set-cur-dir '(0 1 d)))

#|
(defun move ()
  (if current-direction
      (let* ((x-inc (car current-direction))
	     (y-inc (cadr current-direction))
	     (change-dir (nth 2 current-direction))
	     (snek-length (1- (length (snake-segments snek))))
	     (coordinate-change nil)
	     (cur-segment (nth snek-length (snake-segments snek))))
	(if change-dir
	    (setf coordinate-change (list (segment-x (nth snek-length (snake-segments snek)))
					  (segment-y (nth snek-length (snake-segments snek)))
					  x-inc
					  y-inc)))
	(incf (segment-x (nth snek-length (snake-segments snek))) x-inc)
	(incf (segment-y (nth snek-length (snake-segments snek))) y-inc)
	(if (> (segment-x cur-segment) (1- (car game-board)))
	    (setf (segment-x (nth snek-length (snake-segments snek))) 0))
	(if (< (segment-x cur-segment) 0)
	    (setf (segment-x (nth snek-length (snake-segments snek))) (1- (car game-board))))
	(if (> (segment-y cur-segment) (1- (cadr game-board)))
	    (setf (segment-y (nth snek-length (snake-segments snek))) 0))
	(if (< (segment-y cur-segment) 0)
	    (setf (segment-y (nth snek-length (snake-segments snek))) (1- (cadr game-board))))
	(setf (segment-move-state (nth snek-length (snake-segments snek))) current-direction)
;;	      (segment-change-at-coordinate (nth snek-length (snake-segments snek))) coordinate-change)
	(setf (nth 2 current-direction) nil)
	(loop for segment in (cdr (reverse (snake-segments snek)))
	   do (setf (segment-x segment) (+ (segment-x segment) (car (segment-move-state segment))))
	     (setf (segment-y segment) (+ (segment-y segment) (cadr (segment-move-state segment))))
	     (if (not (segment-change-at-coordinate segment))
		 (setf (segment-change-at-coordinate segment) coordinate-change
		       coordinate-change nil))
	     (if (segment-change-at-coordinate segment)
		 (if (and (eq (segment-x segment) (car (segment-change-at-coordinate segment)))
			  (eq (segment-y segment) (cadr (segment-change-at-coordinate segment))))
		     (if coordinate-change
			 (setf (segment-move-state segment) (list (nth 2 (segment-change-at-coordinate segment))
								  (nth 3 (segment-change-at-coordinate segment)))
			       (segment-change-at-coordinate segment) coordinate-change)
			 (setf (segment-move-state segment) (segment-move-state
							     (nth (1+ (position segment (snake-segments snek)))
								  (snake-segments snek)))
			       #|(list (nth 2 (segment-change-at-coordinate segment))
			       (nth 3 (segment-change-at-coordinate segment)))|#
			       coordinate-change (segment-change-at-coordinate segment)
			       (segment-change-at-coordinate segment) (segment-change-at-coordinate
								       (nth (1+ (position segment (snake-segments snek)))
									    (snake-segments snek)))))))
	     (if (not (segment-change-at-coordinate segment))
		 (setf (segment-move-state segment) current-direction))
#|	     (if coordinate-change
		 (if (and (eq (segment-x segment) (car coordinate-change))
			  (eq (segment-y segment) (cadr coordinate-change)))
		     (setf (segment-move-state segment) (list (nth 2 coordinate-change)
							      (nth 3 coordinate-change))
			   coordinate-change (list (segment-x segment)
						   (segment-y segment)
						   (nth 2 coordinate-change)
						   (nth 3 coordinate-change))))
		 (if (and (eq (segment-x segment) (car (segment-change-at-coordinate segment)))
			  (eq (segment-y segment) (cadr (segment-change-at-coordinate segment))))
		     (setf (segment-move-state segment) (list (nth 2 (segment-change-at-coordinate segment))
							      (nth 3 (segment-change-at-coordinate segment))))))|#
	     (if (> (segment-x segment) (1- (car game-board)))
		 (setf (segment-x segment) 0))
	     (if (< (segment-x segment) 0)
		 (setf (segment-x segment) (1- (car game-board))))
	     (if (> (segment-y segment) (1- (cadr game-board)))
		 (setf (segment-y segment) 0))
	     (if (< (segment-y segment) 0)
		 (setf (segment-y segment) (1- (cadr game-board))))))))|#

#|
(defun set-cur-dir (lst)
  (if (< (length (snake-segments snek)) 2)
      (setf current-direction lst)
      (case (nth 3 current-direction)
	(u (if (not (eq (nth 3 lst) 'd))
	       (setf current-direction lst)))
	(l (if (not (eq (nth 3 lst) 'r))
	       (setf current-direction lst)))
	(r (if (not (eq (nth 3 lst) 'l))
	       (setf current-direction lst)))
	(d (if (not (eq (nth 3 lst) 'u))
		 (setf current-direction lst))))))
(add-key :scancode-up snake :down (set-cur-dir '(0 -1 t u)))
(add-key :scancode-left snake :down (set-cur-dir '(-1 0 t l)))
(add-key :scancode-right snake :down (set-cur-dir '(1 0 t r)))
(add-key :scancode-down snake :down (set-cur-dir '(0 1 t d)))|#