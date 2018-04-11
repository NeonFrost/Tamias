(defvar start-accumulator nil)

(defun start-game ()
  (if (not start-accumulator)
      (start-game-music))
  (setf (entity-y player) (round (* (/ *screen-height* 8) 7))
	(entity-x player) (round (/ *screen-width* 2))
	(vector-3d-y (entity-vector player)) 0
	started nil
	start-accumulator 1000
	state 'level)
  (setf (bounding-box-x (entity-bounding-box player)) (entity-x player)
	(bounding-box-y (entity-bounding-box player)) (entity-y player)
	(bounding-box-width (entity-bounding-box player)) (entity-width player)
	(bounding-box-height (entity-bounding-box player)) (entity-height player)))

