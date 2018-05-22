;;;;figure out a decent name for the physics engine part of Tamias, like Elk
(defstruct (joint (:include t-object))
  parent-body ;;'((beginnnig) (end)) ... beginning = ((0) (5)) end = ((3) (4))... we're...going to change that eventually.
  child-body
  (angle 0))
#|
NOTE: change joint and soft-body to soft-body and object-group
reimplement joint
|#
(defstruct (soft-body (:include t-object))
  joints
  shape
  angle)

(defstruct (group (:include t-object))
  bodies)
;;implement constraints

(defmacro create-soft-body (group &key (x 0) (y 0) (width 4) (height 8) (color +white+) (angle 0))
  `(push (make-soft-body :x ,x :y ,y :width ,width :height ,height :color ,color :angle ,angle
			 :texture (create-texture :width ,width :height ,height :color ,color)) (group-bodies ,group)))

(defmacro create-joint (parent-group parent-body &key x y (width 4) (height 8))
  `(let ((x (or ,x
		(soft-body-x ,parent-body)))
	 (y (or ,y
		(soft-body-y ,parent-body))))
     (push (make-joint :x x
		       :y y
		       :width ,width
		       :height ,height
		       :parent-body (list (position ,parent-body ,parent-group)
					  `,',parent-group))
	   (soft-body-joints ,parent-body))))
    
(defun velocity (object &key (dimensions 2))
  (case dimensions
    (1 (list 'x: (vector-3d-x (object-vector object))))
    (2 (list 'x: (vector-3d-x (object-vector object))
	     'y: (vector-3d-y (object-vector object))))
    (3 (list 'x: (vector-3d-x (object-vector object))
	     'y: (vector-3d-y (object-vector object))
	     'z: (vector-3d-z (object-vector object))))))

(defun draw-bodies (group)
  (loop for body in (group-bodies group)
     do (tex-blit (soft-body-texture body)
		  :dest (create-rectangle (list (soft-body-x body)
						(soft-body-y body)
						(soft-body-width body)
						(soft-body-height body)))
		  :angle (soft-body-angle body)
		  :color (soft-body-color body))))

#|
(defun impulse-resolution (soft-body)
  "Deals with resolving impulses on the soft-body."
  )
|#
