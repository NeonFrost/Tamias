(defvar *gravity* 1)
(defvar *fluids* (list '(80 80 240 400 1)))
(defvar *current-fluid* nil)

(defun calculate-force (object)
  (* (object-mass object) (velocity object))
  )

(defun is-submerged (object)
  (let ((bounding-box (t-object-bounding-box object)))
    (loop for fluid in *fluids*
       do (if (and (> (bounding-box-x bounding-box) (car fluid))
		   (> (bounding-box-y bounding-box) (cadr fluid))
		   (< (bounding-box-width bounding-box) (caddr fluid))
		   (< (bounding-box-height bounding-box) (nth 3 fluid)))
	      (progn (setf *current-liquid* fluid)
		     (return t))))))

(defun apply-force (object &key collided-object (force 'acceleration))
  (case force
    (gravity (+ (vector-3d-y (object-vector object)) *gravity*))
    (buoyancy (if (is-submerged object)
		  (progn (decf (vector-3d-y (object-vector object)) (nth 4 *current-liquid*))
			 (setf *current-liquid* nil))
		  ))
    (acceleration (set-vector-3d-values (object-vector object) (object-acceleration object)))
    (friction (set-vector-3d-values (object-vector object) nil :function 'mulitply :scalar (object-friction collided-object)))
    ))
