(defvar human (make-entity :x 8
			   :y (round (- (/ *screen-height* 2) 16))
			   :width 16
			   :height (round (/ *screen-height* 16))
			   :vector (make-vector-3d)
			   :bounding-box (make-bounding-box)
			   :symbol-color +steel-blue+))
(setf (bounding-box-x (entity-bounding-box human)) (entity-x human)
      (bounding-box-y (entity-bounding-box human)) (entity-y human)
      (bounding-box-width (entity-bounding-box human)) (entity-width human)
      (bounding-box-height (entity-bounding-box human)) (entity-height human))
(defvar ai (make-entity :x (- *screen-width* 24)
			:y (round (- (/ *screen-height* 2) 16))
			:width 16
			:height (round (/ *screen-height* 16))
			:vector (make-vector-3d)
			:bounding-box (make-bounding-box)
			:symbol-color +pastel-grey+))
(setf (bounding-box-x (entity-bounding-box ai)) (entity-x ai)
      (bounding-box-y (entity-bounding-box ai)) (entity-y ai)
      (bounding-box-width (entity-bounding-box ai)) (entity-width ai)
      (bounding-box-height (entity-bounding-box ai)) (entity-height ai))
(defvar ball (make-entity :x (round (- (/ *screen-width* 2) 4))
			  :y (round (- (/ *screen-height* 2) 4))
			  :width 8
			  :height 8
			  :vector (make-vector-3d)
			  :bounding-box (make-bounding-box)
			  :symbol-color +white+))
(setf (bounding-box-x (entity-bounding-box ball)) (entity-x ball)
      (bounding-box-y (entity-bounding-box ball)) (entity-y ball)
      (bounding-box-width (entity-bounding-box ball)) (entity-width ball)
      (bounding-box-height (entity-bounding-box ball)) (entity-height ball))
(push (quote human) entities)
(push (quote ai) entities)
(push (quote ball) objects)

(defvar start-accumulator nil)

(defun start-game ()
  (if (not start-accumulator)
      (start-game-music))
  (setf (entity-y human)  (round (- (/ *screen-height* 2) 16))
	(entity-y ai) (round (- (/ *screen-height* 2) 16))
	(entity-x ai) (- *screen-width* 24)
	(vector-3d-y (entity-vector human)) 0
	(vector-3d-y (entity-vector ai)) 0
	(entity-x ball) (round (- (/ *screen-width* 2) 4))
	(entity-y ball) (round (- (/ *screen-height* 2) 4))
	(entity-vector ball) (make-vector-3d)
	started nil
	start-accumulator 1000
	state 'level))
