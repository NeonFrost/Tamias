(defvar current-text-context "")
(defvar *current-text-position* 0)
(defvar *text-input-state* nil)

(defstruct modifier-states
  control
  shift
  meta)
(defvar modifier-states (make-modifier-states))

(defun alt-t ()
  (if (modifier-states-meta modifier-states)
      t
      nil))
(defun meta-t ()
  (if (modifier-states-meta modifier-states)
      t
      nil))
(defun ctrl-t ()
  (if (modifier-states-control modifier-states)
      t
      nil))
(defmacro control-t ()
  (ctrl-t))
(defun shift-t ()
  (if (modifier-states-shift modifier-states)
      t
      nil))

(defun quit-game ()
  (sdl2:push-event :quit)
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
  ;;switch to (sym-value keysym)
  (case key
    ((:scancode-lctrl :scancode-rctrl) (setf (modifier-states-control modifier-states) t))
    ((:scancode-lshift :scancode-rshift) (setf (modifier-states-shift modifier-states) t))
    ((:scancode-lalt :scancode-ralt) (setf (modifier-states-meta modifier-states) t))
    (:scancode-return (if *text-input-state*
			  (progn (setf current-text-context (with-output-to-string (stream)
							      (if (< *current-text-position* (length current-text-context))
								  (progn (write-string (subseq current-text-context 0 (1+ *current-text-position*)) stream)
									 (fresh-line stream)
									 (write-string (subseq current-text-context *current-text-position*) stream))
								  (progn (write-string (subseq current-text-context 0 *current-text-position*) stream)
									 (fresh-line stream)
									 (write-string (subseq current-text-context *current-text-position*) stream)))))
				 (incf *current-text-position* 1))))
    (:scancode-backspace (if (and *text-input-state*
				  (> *current-text-position* 0)
				  (< *current-text-position* (length current-text-context)))
			     (progn (setf current-text-context (with-output-to-string (stream)
								 (write-string (subseq current-text-context 0 (1- *current-text-position*)) stream)
								 (write-string (subseq current-text-context *current-text-position*) stream)))
				    (decf *current-text-position* 1))
			     (if (and *text-input-state*
				      (> *current-text-position* 0))
				 (progn (setf current-text-context (with-output-to-string (stream)
								     (write-string (subseq current-text-context 0 (1- *current-text-position*)) stream)))
					(decf *current-text-position* 1)))))
    (:scancode-right (if *text-input-state*
			 (if (< *current-text-position* (length current-text-context))
			     (incf *current-text-position* 1))))
    (:scancode-left  (if *text-input-state*
			 (if (> *current-text-position* 0)
			     (decf *current-text-position* 1))))
    (:scancode-escape (quit-game))
    (:scancode-grave (if (ctrl-t)
			     (quit-game))))
  (if (gethash key (gethash :down (state-keys (eval state))))
      (loop for func in (gethash key (gethash :down (state-keys (eval state))))
	 do (eval func))))

(defun keyup-check (key)
  (case key
    ((:scancode-lctrl :scancode-rctrl) (setf (modifier-states-control modifier-states) nil))
    ((:scancode-lshift :scancode-rshift) (setf (modifier-states-shift modifier-states) nil))
    ((:scancode-lalt :scancode-ralt) (setf (modifier-states-meta modifier-states) nil))
    )
  (if (gethash key (gethash :up (state-keys (eval state))))
      (loop for func in (gethash key (gethash :up (state-keys (eval state))))
	 do (eval func))))


(defun handle-text-input (text)
  (let ((text (ascii-to-string text)))
    (setf current-text-context (with-output-to-string (stream)
				 (write-string (subseq current-text-context 0 *current-text-position*) stream)
				 (write-string text stream)
				 (write-string (subseq current-text-context *current-text-position*) stream)))
    ;;				 (combine-strings current-text-context text))
    (incf *current-text-position* 1)
    ))
