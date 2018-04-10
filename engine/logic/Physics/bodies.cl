;;;;figure out a decent name for the physics engine part of Tamias, like Elk
(defstruct object
  x
  y
  width
  height
  (vector (make-vector-3d))
  (acceleration (make-vector-3d :z 0))
  (friction 1)
  (mass 1))
(defstruct (joint (:include object))
  )
(defstruct (soft-body (:include object))
  joints
  shape)
(defvar *gravity* 1)

(defun calculate-force (object)
  (* (object-mass object) (velocity object))
  )

(defun apply-force (object &key collided-object (force 'acceleration))
  (case force
    (gravity (- (vector-3d-y (object-vector object)) *gravity*))
    (acceleration (set-vector-3d-values (object-vector object) (object-acceleration object)))
    (friction (set-vector-3d-values (object-vector object) nil :function 'mulitply :scalar (object-friction collided-object)))
    ))
    
(defun velocity (object &key (dimensions 2))
  (case dimensions
    (1 (vector-3d-x (object-vector object)))
    (2 (+ (vector-3d-x (object-vector object))
	  (vector-3d-y (object-vector object))))
    (3 (+ (vector-3d-x (object-vector object))
	  (vector-3d-y (object-vector object))
	  (vector-3d-z (object-vector object))))))

(defun draw-joints (sb)
  (loop for joint in (soft-body-joints sb)
     do (render-box (+ (soft-body-x sb) (joint-x joint))
		    (+ (soft-body-y sb) (joint-y joint))
		    (joint-width joint)
		    (joint-height joint))
       ))

(defun <Place-Holder-name> (soft-body)
  "Deals with moving the soft body, i.e. a rope, and lowering it's speed due to friction"
  )
