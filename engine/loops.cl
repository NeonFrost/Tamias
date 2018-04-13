(defun process-loop ()
  (loop for func in (gethash sub-state (state-loops (eval state)))
     do (funcall func))
  )

(defun pause-game ()
  (setf state 'paused)
  (pause-music))

(defun resume-game ()
  (setf state 'level)
  (resume-level-music))
