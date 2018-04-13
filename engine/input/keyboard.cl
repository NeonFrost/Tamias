(defvar old-sub-state 'top)
(defvar equip-state nil)
(defvar moving nil)
(defstruct modifier-states
  control
  shift
  meta)
(defvar modifier-states (make-modifier-states))

(defun meta-t ()
  (if (modifier-states-meta modifier-states)
      t
      nil))
(defun control-t ()
  (if (modifier-states-control modifier-states)
      t
      nil))
(defun shift-t ()
  (if (modifier-states-shift modifier-states)
      t
      nil))

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

(defvar *selection-column* 0)
(defvar *selection-row* 0)
(defun change-selection (d &key (max-row 2) (max-column 1))
  (case d
    ((0 up) (if (> *selection-row* 0)	   
		(decf *selection-row* 1)
		(setf *selection-row* max-row)))
    ((1 right) (if (< *selection-column* max-column)
		  (incf *selection-column* 1)
		  (setf *selection-column* 0)))
    ((2 down) (if (< *selection-row* max-row)
		  (incf *selection-row* 1)
		  (setf *selection-row* 0)))
    ((3 left) (if (> *selection-column* 0)
		   (decf *selection-column* 1)
		   (setf *selection-column* max-column)))))

(defun keydown-check (key)
  (case key
    ((or :scancode-lctrl :scancode-rctrl) (setf (modifier-states-control modifier-states) t))
    ((or :scancode-lshift :scancode-rshift) (setf (modifier-states-shift modifier-states) t))
    (:scancode-escape (quit-game)))
  (if (gethash key (gethash :down (state-keys (eval state))))
      (loop for func in (gethash key (gethash :down (state-keys (eval state))))
	 do (eval func))))

(defun keyup-check (key)
  (case key
    ((or :scancode-lctrl :scancode-rctrl) (setf (modifier-states-control modifier-states) nil))
    ((or :scancode-lshift :scancode-rshift) (setf (modifier-states-shift modifier-states) nil))
    )
  (if (gethash key (gethash :up (state-keys (eval state))))
      (loop for func in (gethash key (gethash :up (state-keys (eval state))))
	 do (eval func))))
