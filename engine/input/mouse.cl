(defun mouse-button-check (button)
;  (print button)
;  (print tamias:*mouse-x*)
  (let ((button (case button
		  (1 :button-left)
		  (2 :button-right)
		  (4 :button-middle))))
    (tamias-mouse button :down (state-symbol tamias:state) (state-sub-state tamias:state) nil)))
#|    (if (gethash button (gethash :down (state-mouse (eval tamias:state))))
	(loop for func in (gethash button (gethash :down (state-mouse (eval tamias:state))))
	   do (eval func)))))
|#

(defun mouse-button-release-check (button)
  (let ((button (case button
		  (1 :button-left)
		  (2 :button-right)
		  (4 :button-middle))))
    (tamias-mouse button :up (state-symbol tamias:state) (state-sub-state tamias:state) nil)))
#|(if (gethash button (gethash :up (state-mouse (eval tamias:state))))
	(loop for func in (gethash button (gethash :up (state-mouse (eval tamias:state))))
	   do (eval func)))))
|#
;;call trap on state change
(defun trap-mouse-in-window ()
  (sdl2:set-relative-mouse-mode t)
  (sdl2:hide-cursor))
;;call free mouse on state change
(defun free-mouse-from-window ()
  (sdl2:set-relative-mouse-mode nil)
  (sdl2:show-cursor))
;;Justification for this instead of snapping mouse to window on window creation:
;;possible issues with winowing frameworks.


(defun mouse-move (button-state x y xrel yrel)
  (let ((button (case button-state
		  (1 :button-left)
		  (2 :button-right)
		  (4 :button-middle)
		  (otherwise nil))))
    (setf tamias:*mouse-x* x
	  tamias:*mouse-y* y
	  tamias:*mouse-velocity-x* xrel
	  tamias:*mouse-velocity-y* yrel)
    (if button
	(tamias-mouse button :down (state-symbol tamias:state) (state-sub-state tamias:state) t)
	(tamias-mouse button nil (state-symbol tamias:state) (state-sub-state tamias:state) t))))
#|  (if (gethash :move (state-mouse (eval tamias:state)))
      (loop for func in (gethash :move (state-mouse (eval tamias:state)))
do (eval func))))|#


(defmacro test-mouse-width (x-point point-width)
  `(and (>= (- tamias:*mouse-x* tamias:*cursor-size*) (- ,x-point tamias:*cursor-size*))
	(<= (+ tamias:*mouse-x* tamias:*cursor-size*) (+ ,x-point ,point-width tamias:*cursor-size*))))

(defmacro test-mouse-height (y-point point-height)
  `(and (>= (- tamias:*mouse-y* tamias:*cursor-size*) (- ,y-point tamias:*cursor-size*))
	(<= (+ tamias:*mouse-y* tamias:*cursor-size*) (+ ,y-point ,point-height tamias:*cursor-size*))))

(defmacro test-mouse-x (point-x)
  `(and (<= (- tamias:*mouse-x* tamias:*cursor-size*) ,point-x)
	(>= (+ tamias:*mouse-x* tamias:*cursor-size*) ,point-x)))

(defmacro test-mouse-y (point-y)
  `(and (<= (- tamias:*mouse-y* tamias:*cursor-size*) ,point-y)
	(>= (+ tamias:*mouse-y* tamias:*cursor-size*) ,point-y)))

(defun mouse.test-position (x y &optional (width 0) (height 0) (collision-method 'point))
  (case collision-method
    (point (and (test-mouse-x x) (test-mouse-y y)))
    (box (and (test-mouse-width x width) (test-mouse-height y height)))))
