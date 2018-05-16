(define-state buoyancy)
(define-state gravity)
(define-state friction)
(define-state joints)
(defvar brick (make-t-object))
(defvar liquid-encasing (make-t-object :x 80 :y 80 :width 240 :height 400))
(defvar rigid-box (make-t-object))
(defvar rope (make-soft-body :x (round (/ *screen-width* 2))
			     :y (round (/ *screen-height* 4))
			     :width 4
			     :height 2))
(defvar rope-impulse nil)
;;init
(defun buoyancy-init ()
  (if (t-object-texture liquid-encasing)
      (sdl2:destroy-texture (t-object-texture liquid-encasing)))
  (if (t-object-texture rigid-box)
      (sdl2:destroy-texture (t-object-texture rigid-box)))
  (setf (t-object-texture rigid-box) (create-texture :width 64 :height 32)
	(t-object-x rigid-box) 160
	(t-object-y rigid-box) 90
	(t-object-width rigid-box) 64
	(t-object-height rigid-box) 32)
  (setf (t-object-bounding-box rigid-box) (make-bounding-box :x (t-object-x rigid-box)
							     :y (t-object-y rigid-box)
							     :width (t-object-width rigid-box)
							     :height (t-object-height rigid-box))))
(defun gravity-init ()
  (if (t-object-texture brick)
      (sdl2:destroy-texture (t-object-texture brick)))
  (setf (t-object-texture brick) (create-texture :width 64 :height 32)
	(t-object-x brick) 160
	(t-object-y brick) 90
	(t-object-width brick) 64
	(t-object-height brick) 32)
  (setf (t-object-bounding-box brick) (make-bounding-box :x (t-object-x brick)
							 :y (t-object-y brick)
							 :width (t-object-width brick)
							 :height (t-object-height brick))))
(defun friction-init ()
  (if (t-object-texture brick)
      (sdl2:destroy-texture (t-object-texture brick)))
  (setf (t-object-texture brick) (create-texture :width 64 :height 32)
	(t-object-x brick) 160
	(t-object-y brick) 90
	(t-object-width brick) 64
	(t-object-height brick) 32)
  (setf (t-object-bounding-box brick) (make-bounding-box :x (t-object-x brick)
							 :y (t-object-y brick)
							 :width (t-object-width brick)
							 :height (t-object-height brick))))
(defun joints-init ()
  (loop for r below 20
     do (defjoint rope :x (t-object-x rope) :y (+ (t-object-y rope) (* r 2)) :width 4 :height 2))
  )

;;render
(defun buoyancy-render ()
  )
(add-to-state-render buoyancy buoyancy-render)
(defun gravity-render ()
  )
(add-to-state-render gravity gravity-render)
(defun friction-render ()
  )
(add-to-state-render friction friction-render)
(defun joints-render ()
  (draw-joints rope))
(add-to-state-render joints joints-render)

;;loops
(defun joints-loop ()
  (if rope-impulse
      (loop for joint in (soft-body-joints rope)
	 do ;;change rotation of each joint
	   ;;check where the 
	   ))
  )

;;Input
