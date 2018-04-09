(defvar old-sub-state 'top)
(defvar equip-state nil)
(defvar moving nil)
(defstruct modifier-states
  control
  shift
  meta)
(defvar modifier-states (make-modifier-states))

(defun quit-game ()
  (sdl2:push-event :quit)
  )

(defun exit-to-main-menu ()
  (setf state 'title)
  (setf selection 0)
  (sdl2-mixer:halt-music)
  (start-main-menu-music (track-path main-menu-track))
  )

(defun go-to-options ()
  (setf selection 0)
  (setf sub-state 'options))

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

(defun main-menu-keys-down (key)
  (case sub-state
    (top (case key
	   (:scancode-up (change-selection 0))
	   (:scancode-down (change-selection 1))))
    (options (case key
	       (:scancode-up (change-selection 0 :max-length 2))
	       (:scancode-down (change-selection 1 :max-length 2))
	       (:scancode-right (case selection
				  (0 (if (< max-volume 124)
					 (progn (incf max-volume 5)
						(setf +track-volume+ max-volume)
						(sdl2-mixer:volume-music +track-volume+))))
				  (1 (incf resolution 1)
				     (if (> resolution (1- (length resolution-list)))
					 (setf resolution 0))
				     (setf *screen-width* (car (nth resolution resolution-list))
					   *screen-height* (cadr (nth resolution resolution-list)))
				     (update-window-size))))
	       (:scancode-left (case selection
				 (0 (if (> max-volume 5)
					(progn (decf max-volume 5)
					       (setf +track-volume+ max-volume)
					       (sdl2-mixer:volume-music +track-volume+))))
				 (1 (decf resolution 1)
				    (if (< resolution 0)
					(setf resolution (1- (length resolution-list))))
				    (setf *screen-width* (car (nth resolution resolution-list))
					  *screen-height* (cadr (nth resolution resolution-list)))
				    (update-window-size))))
	       ))
    ))

(defun main-menu-keys-up (key)
  (case key
    (:scancode-return (confirm-selection))
    (:scancode-escape (quit-game))
    (:scancode-z (confirm-selection))
    ))

(defun paused-keys-down (key)
  (case key
    (:scancode-up (change-selection 0))
    (:scancode-down (change-selection 1))
    ))

(defun paused-keys-up (key)
  (case key
    (:scancode-z (confirm-selection))
    (:scancode-x (case sub-state
		   (options (setf sub-state nil)
			    (setf selection 3))
		   (scores (setf sub-state nil)
			   (setf selection 2))
		   (otherwise (setf state 'level)
			      (setf selection 0))
		   ))
    ))

(defun level-keys-down (key)
  (case key
    (:scancode-return (pause-game))
    (:scancode-left (setf moving 'left))
    (:scancode-right (setf moving 'right))
    ))

(defun level-keys-up (key)
  (case key
    (:scancode-left (stop-moving))
    (:scancode-right (stop-moving))
    ))

(defun game-over-keys-down (key)
  )

(defun game-over-keys-up (key)
  (case key
    (:scancode-z (exit-to-main-menu))
    ))

(defun change-selection (d &key (max-length 2))
  (case d
    ((0 up) (if (> selection 0)	   
		(decf selection 1)
		(setf selection max-length)))
    ((1 down) (if (< selection max-length)
		  (incf selection 1)
		  (setf selection 0)))
    ))

(defun keydown-check (key)
  (case key
    ((or :scancode-lctrl :scancode-rctrl) (setf (modifier-states-control modifier-states) t))
    ((or :scancode-lshift :scancode-rshift) (setf (modifier-states-shift modifier-states) t))
    (:scancode-escape (quit-game))
    )
  (if (assoc key (cadr (assoc :down (state-keys (eval state)))))
      (eval (cadr (assoc key (cadr (assoc :down (state-keys (eval state)))))))))
#|  (case state
    (title (main-menu-keys-down key))
    (paused (paused-keys-down key))
    (level (level-keys-down key))
    (game-over (game-over-keys-down key))
    ))|#

(defun keyup-check (key)
  (case key
    ((or :scancode-lctrl :scancode-rctrl) (setf (modifier-states-control modifier-states) nil))
    ((or :scancode-lshift :scancode-rshift) (setf (modifier-states-shift modifier-states) nil))
    )
  (if (assoc key (cadr (assoc :up (state-keys (eval state)))))
      (eval (cadr (assoc key (cadr (assoc :up (state-keys (eval state)))))))))
#|  (case state
    (title (main-menu-keys-up key))
    (paused (paused-keys-up key))
    (level (level-keys-up key))
    (game-over (game-over-keys-up key))
    ))|#

