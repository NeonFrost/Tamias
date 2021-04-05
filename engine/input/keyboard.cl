(defstruct tamias-value
  type
  value);;for use with editable non-string fields

(defstruct tamias-text
  (text " ")
  (position 1))

(defvar current-text-context nil)
(defvar *text-input-state* nil)

(defun handle-text-input (text)
  (let ((text (tamias.string:ascii-to-string text)))
    (setf current-text-context (with-output-to-string (stream)
				 (write-string (subseq current-text-context 0 (tamias-text-position current-text-context)) stream)
				 (write-string text stream)
				 (write-string (subseq current-text-context (tamias-text-position current-text-context)) stream)))
    ;;				 (combine-strings current-text-context text))
    (incf (tamias-text-position current-text-context) 1)
    ))

(defstruct modifier-states
  control
  shift
  meta)
(defvar modifier-states (make-modifier-states))

(defun alt-t ()
  (if (modifier-states-meta modifier-states)
      t
      nil))
(defmacro meta-t ()
  (alt-t))
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

#|
(defun go-to-options ()
  (setf tamias:selection 0)
  (setf tamias:sub-state 'options))


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
|#

(defun keydown-check (key)
  (let ((key (intern (substitute #\- #\space (string-upcase key)) "KEYWORD")))
;    (print key)
    (case key
      (:ctrl ;;(or :Left-Ctrl :Right-Ctrl)
       (setf (modifier-states-control modifier-states) t))
      (:shift ;;(or :Left-Shift :Right-Shift)
       (setf (modifier-states-shift modifier-states) t))
      (:alt (setf (modifier-states-meta modifier-states) t))
      (:return (if *text-input-state*
		   (progn (setf current-text-context (with-output-to-string (stream)
						     (if (< (tamias-text-position current-text-context) (length current-text-context))
							 (progn (write-string (subseq current-text-context 0 (1+ (tamias-text-position current-text-context))) stream)
								(fresh-line stream)
								(write-string (subseq current-text-context (tamias-text-position current-text-context)) stream))
							 (progn (write-string (subseq current-text-context 0 (tamias-text-position current-text-context)) stream)
								(fresh-line stream)
								(write-string (subseq current-text-context (tamias-text-position current-text-context)) stream)))))
			(incf (tamias-text-position current-text-context) 1))))
      (:backspace (if (and *text-input-state*
			   (> (tamias-text-position current-text-context) 0)
			   (< (tamias-text-position current-text-context) (length current-text-context)))
		      (progn (setf current-text-context (with-output-to-string (stream)
							  (write-string (subseq current-text-context 0 (1- (tamias-text-position current-text-context))) stream)
							  (write-string (subseq current-text-context (tamias-text-position current-text-context)) stream)))
			     (decf (tamias-text-position current-text-context) 1))))
#|		      (if (and *text-input-state*
			       (> *current-text-position* 0))
			  (progn (setf current-text-context (with-output-to-string (stream)
							      (write-string (subseq current-text-context 0 (1- *current-text-position*)) stream)))
				 (decf *current-text-position* 1)))))|#
      (:right (if *text-input-state*
		  (if (< (tamias-text-position current-text-context) (length current-text-context))
		      (incf (tamias-text-position current-text-context) 1))))
      (:left  (if *text-input-state*
		  (if (> (tamias-text-position current-text-context) 0)
		      (decf (tamias-text-position current-text-context) 1))))
      (:Escape (quit-game))
      (:|`| (if (ctrl-t)
		(quit-game))))
    (if (not *text-input-state*)
	(tamias-key key (state-symbol tamias:state) tamias:sub-state :down (ctrl-t) (alt-t) (shift-t)))))
	#|(and (not *text-input-state*)
	     (gethash key (gethash :down (state-keys (eval tamias:state)))))
	(loop for func in (gethash key (gethash :down (state-keys (eval tamias:state))))
     do (eval func)))))|#
     

#|
blinking cursor:
(defstruct text-cursor
  (bliink-counter 0)
  blink?
  current-vis)
(defvar blink-counter 0)
(defvar blink? nil)

(defun blinking-cursor ()
  (if (not blink?)
      (render:box [current-context-x] [current-context-y] 2 character-height :color tamias.colors:+black+)))
(defun blink-counter (cursor)
  (incf (text-curosr-blink-counter text-cursor))
  (if (>= blink-counter [frames])
      (setf blink-counter 0
	    blink? (not blink?))))

|#

#|(defun keydown-check (key)
  (case key
    ((or :scancode-lctrl :scancode-rctrl) (setf (modifier-states-control modifier-states) t))
    ((or :scancode-lshift :scancode-rshift) (setf (modifier-states-shift modifier-states) t))
    ((or :scancode-lalt :scancode-ralt) (setf (modifier-states-meta modifier-states) t))
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
	 do (eval func))))|#

(defun keyup-check (key)
  (let ((key (intern (substitute #\- #\space (string-upcase key)) "KEYWORD")))
    (case key
      (:ctrl ;;(or :left-ctrl :right-ctrl)
       (setf (modifier-states-control modifier-states) nil))
      (:shift ;;(or :left-shift :right-shift)
       (setf (modifier-states-shift modifier-states) nil))
      (:alt ;;(or :left-alt :right-alt)
       (setf (modifier-states-meta modifier-states) nil))
      )
    (tamias-key key (state-symbol tamias:state) tamias:sub-state :up (ctrl-t) (alt-t) (shift-t))))

#|
    (if (gethash key (gethash :up (state-keys (eval tamias:state))))
	(loop for func in (gethash key (gethash :up (state-keys (eval tamias:state))))
	   do (eval func)))))

|#
