(defun render-state ()
  (loop for render-function in (state-render-list (eval state))
     do (funcall render-function)))
;;  (funcall (state-render-list (eval state))))

(defvar +transition-box-alpha+ 0)
(defvar +transition-state+ 'to)
(defun process-changing-state ()
  (if (and changing-state
	   (state-transition (eval state))
	   (state-transition (eval changing-state)))
      (progn (render-box 0
			 0
			 *screen-width*
			 *screen-height*
			 :color (0 0 0 +transition-box-alpha+))
	     (if (eq +transition-state+ 'to)
		 (incf +transition-box-alpha+ 12)
		 (decf +transition-box-alpha+ 12))
	     (if (< +transition-box-alpha+ 0)
		 (setf +transition-box-alpha+ 0))
	     (if (> +transition-box-alpha+ 255)
		 (setf +transition-box-alpha+ 255))
	     (if (eq +transition-box-alpha+ 255)
		 (progn (setf +transition-state+ 'from+)
			(if (eq state 'title)
			    (start-game))
			(setf state changing-state)))
	     (if (eq +transition-box-alpha+ 0)
		 (progn (setf changing-state nil))))
      (if changing-state
	  (setf state changing-state))))
