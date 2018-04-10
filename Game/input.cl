;;title-screen
(add-key :scancode-up title :down (case sub-state
				    (top (change-selection 0 :max-row 2))
				    (options (change-selection 0 :max-row 2))))
(add-key :scancode-down title :down (case sub-state
				      (top (change-selection 2 :max-row 2))
				      (options (change-selection 2 :max-row 2))))
(add-key :scancode-right title :down (if (eq sub-state 'options)
					 (case selection-row
					   (0 (if (< max-volume 124)
						  (raise-volume 5)))
					   (1 (incf resolution 1)
					      (if (> resolution (1- (length resolution-list)))
						  (setf resolution 0))
					      (setf *screen-width* (car (nth resolution resolution-list))
						    *screen-height* (cadr (nth resolution resolution-list)))
					      (update-window-size)))))
(add-key :scancode-left title :down (if (eq sub-state 'options)
					(case selection-row
					  (0 (if (> max-volume 5)
						 (lower-volume 5)))
					  (1 (decf resolution 1)
					     (if (< resolution 0)
						 (setf resolution (1- (length resolution-list))))
					     (setf *screen-width* (car (nth resolution resolution-list))
						   *screen-height* (cadr (nth resolution resolution-list)))
					     (update-window-size)))))
(add-key :scancode-z title :up (confirm-selection))
(add-key :scancode-return title :up (confirm-selection))
;;pause
(add-key :scancode-up paused :down (change-selection 'up))
(add-key :scancode-down paused :down (change-selection 'down))
(add-key :scancode-z paused :up (confirm-selection))
(add-key :scancode-x paused :up (case sub-state
				  (options (setf sub-state nil)
					   (setf selection-row 3))
				  (scores (setf sub-state nil)
					  (setf selection-row 2))
				  (otherwise (setf state 'level)
					     (setf selection-row 0))))
;;level
(add-key :scancode-return level :down (pause-game))
(add-key :scancode-left level :down (setf (vector-3d-x (entity-vector player)) -8))
(add-key :scancode-right level :down (setf (vector-3d-x (entity-vector player)) 8))
(add-key :scancode-left level :up (stop-moving))
(add-key :scancode-right level :up (stop-moving))
;;game-over
(add-key :scancode-z game-over :up (exit-to-main-menu))

(defun confirm-selection ()
  (case state
    (title (case sub-state
	     (top (case selection
		    (0 (start-game))
		    (1 (go-to-options))
		    (2 (quit-game))))
	     (options (if (eq selection 2)
			  (progn (setf selection 0)
				 (setf sub-state 'top))))))
    (paused (case selection
	      (0 (resume-game))
	      (1 (exit-to-main-menu))
	      (2 (quit-game))))
    ))
