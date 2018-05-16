(defstruct vector-3d
  (x 0)
  (y 0)
  (z 1))

(defun vector-3d-add (vec1 vec2)
  (values (+ (vector-3d-x vec1) (vector-3d-x vec2))
	  (+ (vector-3d-y vec1) (vector-3d-y vec2))
	  (+ (vector-3d-z vec1) (vector-3d-z vec2))))

(defun vector-3d-sub (vec1 vec2)
  (values (- (vector-3d-x vec1) (vector-3d-x vec2))
	  (- (vector-3d-y vec1) (vector-3d-y vec2))
	  (- (vector-3d-z vec1) (vector-3d-z vec2))))

(defun scalar-multiply (vec1 scalar)
  (values (if (> (vector-3d-x vec1) 0)
	      (floor (* (vector-3d-x vec1) scalar))
	      (ceiling (* (vector-3d-x vec1) scalar)))
	  (if (> (vector-3d-y vec1) 0)
	      (floor (* (vector-3d-y vec1) scalar))
	      (ceiling (* (vector-3d-y vec1) scalar)))
	  (if (> (vector-3d-z vec1) 0)
	      (floor (* (vector-3d-z vec1) scalar))
	      (ceiling (* (vector-3d-z vec1) scalar)))
	  ))

(defun vector-3d-dot (vector vecN)
  (+ (* (vector-3d-x vector) (vector-3d-x vecN))
     (* (vector-3d-y vector) (vector-3d-y vecN))
     (* (vector-3d-z vector) (vector-3d-z vecN))))


(defun set-vector-3d-values (vector vecN &key (function 'add) (scalar 1))
  (case function
    (add (setf (values (vector-3d-x vector) (vector-3d-y vector) (vector-3d-z vector))
	       (vector-3d-add vector vecN)))
    (subtract (setf (values (vector-3d-x vector) (vector-3d-y vector) (vector-3d-z vector))
		    (vector-3d-sub vector vecN)))
    (multiply (setf (values (vector-3d-x vector) (vector-3d-y vector) (vector-3d-z vector))
		    (scalar-multiply vector scalar)))))
