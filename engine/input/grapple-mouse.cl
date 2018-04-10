(defun mouse-button-check (button)
  (if (assoc button (cadr (assoc :down (state-mouse (eval state)))))
      (loop for func in (cadr (assoc button (cadr (assoc :down (state-mouse (eval state))))))
	   do (eval func)))
#|  (case button
    (:sdl-button-left (grapple-fire))
    (:sdl-button-right (setf grapple-points (remove (last grapple-points) grapple-points))))|#
  )
