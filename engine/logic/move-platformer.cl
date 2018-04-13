;;;;use vectors
;;;;SHeet orientation Down, up, right, left
(defun move-entity (entity)
  (incf (entity-x entity) (vector-3d-x (entity-vector entity)))
  (incf (entity-y entity) (vector-3d-y (entity-vector entity)))
  (setf (bounding-box-x (entity-bounding-box entity)) (entity-x entity)
	(bounding-box-y (entity-bounding-box entity)) (entity-y entity)
	(bounding-box-width (entity-bounding-box entity)) (entity-width entity)
	(bounding-box-height (entity-bounding-box entity)) (entity-height entity))
  (if (entity-sprite-sheet entity)
      (progn (incf cell-accumulator 10)
	     (if (> cell-accumulator 10)
		 (let* ((columns (round (/ (sprite-sheet-width (entity-sprite-sheet entity)) (nth 2 (nth 0 (sprite-sheet-cells (entity-sprite-sheet entity)))))))
			(current-row (* (truncate (entity-current-cell entity) columns) columns)))
		   (if (or (> (vector-3d-x (entity-vector entity)) 0)
			   (> (vector-3d-y (entity-vector entity)) 0))
		       (incf (entity-current-cell entity) 1))
		   (if (> (* (truncate (entity-current-cell entity) columns) columns)
			  current-row)
		       (if (> (vector-3d-x (entity-vector entity))
			      (vector-3d-y (entity-vector entity)))
			   (if (> (vector-3d-x (entity-vector entity)) 0)
			       (setf (entity-current-cell entity) (* columns 2))
			       (setf (entity-current-cell entity) (* columns 3)))
			   (if (> (vector-3d-y (entity-vector entity)) 0)
			       (setf (entity-current-cell entity) 0)
			       (setf (entity-current-cell entity) columns)))
		   (setf cell-accumulator 0)))))))

#|  (case moving
    (left (incf cell-accumulator 10)
	  (if (> (entity-x player) (- 0 (/ 310 5)))
	      (decf (entity-x player) 16))
	  (if (> cell-accumulator 10)
	      (progn (if (and (> (entity-current-cell player) 3)
			      (< (entity-current-cell player) 7))
			 (incf (entity-current-cell player) 1)
			 (setf (entity-current-cell player) 4))
		     (setf cell-accumulator 0))))
    (right (incf cell-accumulator 10)
	   (if (< (entity-x player) (- *screen-width* (round (/ (entity-width player) 2))))
	       (incf (entity-x player) 16))
	   (if (> cell-accumulator 10)
	       (progn (if (< (entity-current-cell player) 3)
			     (incf (entity-current-cell player) 1)
			     (setf (entity-current-cell player) 0))
			 (setf cell-accumulator 0))))
    ))|#

(defun stop-moving (entity)
  (setf (vector-3d-x (entity-vector entity)) 0
	(vector-3d-y (entity-vector entity)) 0)
  (if (entity-sprite-sheet entity)
      (let* ((columns (round (/ (sprite-sheet-width (entity-sprite-sheet entity)) (nth 2 (nth 0 (sprite-sheet-cells (entity-sprite-sheet entity))))))))
	(setf (entity-current-cell entity) (truncate (entity-current-cell entity) columns)))))
