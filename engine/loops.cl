(defun process-loop ()
  (tamias-logic (state-symbol tamias:state) (state-sub-state tamias:state))
#|  (loop for func in (gethash tamias:sub-state (state-loops (eval tamias:state)))
     do (funcall func))
|#
  )

(defun pause-game ()
  (setf tamias:state 'paused)
  (pause-music))

(defun resume-game ()
  (setf tamias:state 'level)
  (resume-music))
