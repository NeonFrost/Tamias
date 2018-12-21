(defvar start-accumulator nil)

(defun start-game ()
  (if *current-track*
      (switch-track-to level-track)
      (start-music level-track))
  (setf volume-state 'increasing)
  (setf (entity-x player) (round (/ *screen-width* 2))
	(entity-y player) (round (* (/ *screen-height* 8) 7))

	(entity-x game-floor) (round (/ *screen-width* 8))
	(entity-y game-floor) (round (* (/ *screen-height* 8) 7))
	
	(entity-x grapple-wall-1) (round (/ *screen-width* 8))
	(entity-y grapple-wall-1) (round (/ *screen-height* 8))

	(entity-x grapple-wall-2) (round (* (/ *screen-width* 8) 7))
	(entity-y grapple-wall-2) (round (/ *screen-height* 8))

	(entity-width game-floor) (round (* (/ *screen-width* 8) 6))
	(entity-height game-floor) (round (/ *screen-height* 24))
	
	(entity-width grapple-wall-1) (round (/ *screen-width* 16))
	(entity-height grapple-wall-1) (round (* (/ *screen-height* 8) 5))

	(entity-width grapple-wall-2) (round (/ *screen-width* 16))
	(entity-height grapple-wall-2) (round (* (/ *screen-height* 8) 5))
	
	(vector-3d-y (entity-vector player)) 0
	started nil
	start-accumulator 1000
	state 'level)
  (setf (bounding-box-x (entity-bounding-box player)) (entity-x player)
	(bounding-box-y (entity-bounding-box player)) (entity-y player)
	(bounding-box-width (entity-bounding-box player)) (entity-width player)
	(bounding-box-height (entity-bounding-box player)) (entity-height player)))

