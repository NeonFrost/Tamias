;;;;figure out a decent name for the physics engine part of Tamias, like Elk
(defstruct (joint (:include t-object))
  texture
  connections ;;'((beginnnig) (end)) ... beginning = ((0) (5)) end = ((3) (4))... we're...going to change that eventually.
  (angle 0)
  color)
#|
NOTE: change joint and soft-body to soft-body and object-group
reimplement joint
|#
(defstruct (soft-body (:include t-object))
  joints
  shape)

(defmacro defjoint (soft-body &key x y (width 4) (height 8) (color +white+) (angle 180))
  `(let ((x (or ,x
		(soft-body-x ,soft-body)))
	 (y (or ,y
		(soft-body-y ,soft-body))))
     (push (make-joint :x x
		       :y y
		       :width ,width
		       :height ,height
		       :texture (create-texture :width ,width :height ,height :color ,color)
		       :color ,color) (soft-body-joints ,soft-body))))
    
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
     do (tex-blit (joint-texture joint)
		  :dest (create-rectangle (list (joint-x joint)
						(joint-y joint)
						(joint-width joint)
						(joint-height joint)))
		  :angle (joint-angle joint)
		  :color (joint-color joint))))

#|(defun impulse-resolution (soft-body)
  "Deals with resolving impulses on the soft-body."
  )
|#
