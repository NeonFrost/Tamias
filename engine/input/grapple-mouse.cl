(defun mouse-button-check (button)
  (case button
    (:sdl-button-left (grapple-fire 'mouse-state))
    (:sdl-button-right (setf grapple-points (remove (last grapple-points) grapple-points)))
    ))
