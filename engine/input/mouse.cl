(defun mouse-button-check (button)
  (if (assoc button (cadr (assoc :down (state-mouse (eval state)))))
      (loop for func in (cadr (assoc button (cadr (assoc :down (state-mouse (eval state))))))
	 do (eval func))))

(defun mouse-button-release-check (button)
  (if (assoc button (cadr (assoc :up (state-mouse (eval state)))))
      (loop for func in (cadr (assoc button (cadr (assoc :down (state-mouse (eval state))))))
	 do (eval func))))
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *mouse-velocity-x* 0)
(defvar *mouse-velocity-y* 0)
(defun mouse-move (button-state x y xrel yrel)
  (set *mouse-x* x
       *mouse-y* y
       *mouse-velocity-x* x-rel
       *mouse-velocity-y* y-rel)
  (if (assoc button (cadr (assoc :move (state-mouse (eval state)))))
      (loop for func in (cadr (assoc button (cadr (assoc :move (state-mouse (eval state))))))
	 do (eval func))))
