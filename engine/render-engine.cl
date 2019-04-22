(defun render-state ()
  (loop for render-function in (state-render-list (eval state))
     do (funcall render-function)))

(defvar +transition-box-alpha+ 0)
(defvar +transition-state+ 'to)
(defun process-changing-state ()
  (if changing-state
      (if (and (state-transition (eval state))
	       (state-transition (eval changing-state)))
	  (progn (render-box 0
			     0
			     *screen-width*
			     *screen-height*
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
				  state changing-state)
			    #|(if (eq state 'level)
				(start-game))
			    commenting this out shouldn't affect anything
			    but, every change breaks something|#
			    ))
		 (if (eq +transition-box-alpha+ 0)
		     (setf changing-state nil
			   +transition-state+ 'to)))
	  (setf state changing-state
		changing-state nil))))
