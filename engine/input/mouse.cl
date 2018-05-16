(defun mouse-button-check (button)
  (let ((button (case button
		  (1 :button-left)
		  (2 :button-middle)
		  (3 :button-right))))
    (if (gethash button (gethash :down (state-mouse (eval state))))
	(loop for func in (gethash button (gethash :down (state-mouse (eval state))))
	   do (eval func)))))

(defun mouse-button-release-check (button)
  (let ((button (case button
		  (1 :button-left)
		  (2 :button-middle)
		  (3 :button-right))))
    (if (gethash button (gethash :up (state-mouse (eval state))))
	(loop for func in (gethash button (gethash :up (state-mouse (eval state))))
	   do (eval func)))))
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *mouse-velocity-x* 0)
(defvar *mouse-velocity-y* 0)
(defun mouse-move (button-state x y xrel yrel)
  (setf *mouse-x* x
	*mouse-y* y
	*mouse-velocity-x* xrel
	*mouse-velocity-y* yrel)
  (if (gethash :move (state-mouse (eval state)))
      (loop for func in (gethash :move (state-mouse (eval state)))
	   do (eval func))))
