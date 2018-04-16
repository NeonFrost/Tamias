(defun grapple-fire ()
  (if (and (not +current-grapple-point+)
	   (not +pulling-player+))
      (let ((x nil)
	    (y nil))
	(stop-moving player)
	(setf (values x y) (sdl2:mouse-state))
	;;;;put in a test for whether or not a grappleable surface is at x & y
	;;;;if it passes then do stuff below
	;;;;if not then nothing
	;;;;eventually add in gravity and keychecking to not have gravity affect player (while grappled)
	(setf (vector-3d-x (player-grapple-position player)) (entity-x player)
	      (vector-3d-y (player-grapple-position player)) (entity-y player)
	      (vector-3d-x (player-grapple-vector player)) (round (/ (- x (entity-x player)) 5))
	      (vector-3d-y (player-grapple-vector player)) (round (/ (- y (entity-y player)) 5))
	      +current-grapple-point+ (list x y)))))
(defun reset-grapple ()
  (setf +current-grapple-point+ nil
	+pulling-player+ nil
	+grapple-animation+ nil
	(vector-3d-x (player-grapple-vector player)) 0
	(vector-3d-y (player-grapple-vector player)) 0)
  (stop-moving player))

(defun process-grapples ()
  (if +pulling-player+
      (progn (if (and (eq (entity-x player) (vector-3d-x (player-grapple-position player)))
		      (eq (entity-y player) (vector-3d-y (player-grapple-position player))))
		 (progn (setf +pulling-player+ nil
			      (vector-3d-x (player-grapple-vector player)) 0
			      (vector-3d-y (player-grapple-vector player)) 0)
			(stop-moving player))
		 )))
  (if +current-grapple-point+
      (progn (if (not +grapple-animation+)
		 (setf +grapple-animation+ 0))
	     (if (>= +grapple-animation+ 50)
		 (progn (setf +current-grapple-point+ nil
			      +pulling-player+ t
			      (entity-vector player) (make-vector-3d :x (vector-3d-x (player-grapple-vector player))
								     :y (vector-3d-y (player-grapple-vector player))))
			(setf +grapple-animation+ nil))
		 (progn (incf (vector-3d-x (player-grapple-position player)) (vector-3d-x (player-grapple-vector player)))
			(incf (vector-3d-y (player-grapple-position player)) (vector-3d-y (player-grapple-vector player)))
			(incf +grapple-animation+ 10))))))
(add-loop-function process-grapples level 'top)

(defun render-grapple ()
  (if (or +current-grapple-point+
	  +pulling-player+)
      (draw-line (entity-x player) (entity-y player)
		 (vector-3d-x (player-grapple-position player))
		 (vector-3d-y (player-grapple-position player))
		 :color +dark-pastel-red+)))
#|(tex-blit (player-grapple-texture player)
:dest (sdl2:make-rect (entity-x player)
(entity-y player)
w
h)
:angle (round (find-angle (vector-3d-x (player-grapple-vector player))
			  (vector-3d-y (player-grapple-vector player))))))))|#
(add-to-state-render render-grapple level)
