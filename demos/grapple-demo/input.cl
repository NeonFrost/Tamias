;;title-screen
(add-key :up title :down (case sub-state
				    (top (change-selection 0 :max-row 2))
				    (options (change-selection 0 :max-row 2))))
(add-key :down title :down (case sub-state
				      (top (change-selection 2 :max-row 2))
				      (options (change-selection 2 :max-row 2))))
(add-key :right title :down (if (eq sub-state 'options)
					 (case *selection-row*
					   (0 (if (< max-volume 124)
						  (raise-volume 5)))
					   (1 (incf resolution 1)
					      (if (> resolution (1- (length resolution-list)))
						  (setf resolution 0))
					      (setf *screen-width* (car (nth resolution resolution-list))
						    *screen-height* (cadr (nth resolution resolution-list)))
					      (update-window-size)))))
(add-key :left title :down (if (eq sub-state 'options)
					(case *selection-row*
					  (0 (if (> max-volume 5)
						 (lower-volume 5)))
					  (1 (decf resolution 1)
					     (if (< resolution 0)
						 (setf resolution (1- (length resolution-list))))
					     (setf *screen-width* (car (nth resolution resolution-list))
						   *screen-height* (cadr (nth resolution resolution-list)))
					     (update-window-size)))))
(add-key :z title :up (confirm-selection))
(add-key :return title :up (confirm-selection))
(add-key :t title :down (if (meta-t)
				     (progn (setf *text-input-state* t
						  *current-text-position* (length current-text-context))
					    (sdl2:start-text-input))))
(add-key :r title :down (if (meta-t)
				     (progn (setf *text-input-state* nil)
					    (sdl2:stop-text-input))))
;;pause
(add-key :up paused :down (change-selection 'up))
(add-key :down paused :down (change-selection 'down))
(add-key :z paused :up (confirm-selection))
(add-key :x paused :up (case sub-state
				  (options (setf sub-state nil)
					   (setf *selection-row* 3))
				  (scores (setf sub-state nil)
					  (setf *selection-row* 2))
				  (otherwise (setf state 'level)
					     (setf *selection-row* 0))))
;;level
(add-key :return level :up (pause-game))
(add-key :c level :down (reset-grapple))
(add-key :left level :down (if (not (or +pulling-player+
						 +current-grapple-point+))
					(setf (vector-3d-x (entity-vector player)) -8)))
(add-key :right level :down (if (not (or +pulling-player+
						  +current-grapple-point+))
					 (setf (vector-3d-x (entity-vector player)) 8)))
#|(add-key :left level :up (if (not (or +pulling-player+
					       +current-grapple-point+))
				      (stop-moving player)))
(add-key :right level :up (if (not (or +pulling-player+
						+current-grapple-point+))
				       (stop-moving player)))|#
(add-mouse :button-left level :down (grapple-fire))
(add-mouse :button-right level :down (decf +grapple-points+ 1))
;;game-over
(add-key :z game-over :up (exit-to-main-menu))

(defun confirm-selection ()
  (case state
    (title (case sub-state
	     (top (case *selection-row*
		    (0 (setf changing-state 'level
			     volume-state 'decreasing));;(start-game))
		    (1 (go-to-options))
		    (2 (quit-game))))
	     (options (if (eq *selection-row* 2)
			  (progn (setf *selection-row* 0)
				 (setf sub-state 'top))))))
    (paused (case *selection-row*
	      (0 (resume-game))
	      (1 (exit-to-main-menu))
	      (2 (quit-game))))))
