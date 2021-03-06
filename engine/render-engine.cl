(defun render-state ()
#|  (loop for render-function in (state-render-list (eval tamias:state))
  do (funcall render-function))|#
  (tamias-render (state-symbol tamias:state) (state-sub-state tamias:state))
  (if tamias:messages
      (progn (loop for n below (length tamias:messages)
		do (render:render-string (nth n tamias:messages) 0 (* n 16) :color tamias:messages-color))
	     (incf tamias:messages-counter 1)
	     (if (> tamias:messages-counter 3000)
		 (setf tamias:messages nil
		       tamias:messages-counter 0)))))

(defvar +transition-box-alpha+ 0)
(defvar +transition-state+ 'to)
(defun process-changing-state ()
  (if tamias:changing-state
      (if (and (state-transition (eval tamias:state))
	       (state-transition (eval tamias:changing-state)))
	  (progn (render:box 0
			     0
			     tamias:screen-width
			     tamias:screen-height
			     :color (list 0 0 0 +transition-box-alpha+))
		 (if (eq +transition-state+ 'to)
		     (incf +transition-box-alpha+ 12)
		     (decf +transition-box-alpha+ 12))
		 (if (< +transition-box-alpha+ 0)
		     (setf +transition-box-alpha+ 0))
		 (if (> +transition-box-alpha+ 255)
		     (setf +transition-box-alpha+ 255))
		 (if (eq +transition-box-alpha+ 255)
		     (progn (setf +transition-state+ 'from
				  tamias:state tamias:changing-state)
			    #|(if (eq state 'level)
				(start-game))
			    commenting this out shouldn't affect anything
			    but, every change breaks something|#
			    ))
		 (if (eq +transition-box-alpha+ 0)
		     (setf tamias:changing-state nil
			   +transition-state+ 'to)))
	  (setf tamias:state tamias:changing-state
		tamias:changing-state nil))))
