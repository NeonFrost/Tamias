(defstruct ibox
  (x 0)
  (y 0)
  (width 16)
  (height 16)
  mouse?
  active?)

(defstruct state
;;  (loops (make-hash-table))
;;  (keys (make-hash-table))
  ;;  (mouse (make-hash-table))
  symbol
  (sub-state 'top)
  (transition t)
  (ibox (make-ibox))
;;  (render-list '())
  )

#|
(defun concatenate-symbols (&rest objects)
  (intern (apply #'concatenate 'string (mapcar #'princ-to-string objects))))
|#
(defun set-sub-state (sub-state)
  (setf (state-sub-state tamias:state) sub-state))
(defun get-sub-state ()
  (state-sub-state tamias:state))
(defmacro define-state (state)
  `(progn (defvar ,state (make-state
			  :symbol ',state))
	  (push ',state states)
;;	  (init-input ,state)
	  ))
(defmacro set-state (state &optional sub-state)
  `(progn (setf tamias:state ,state)
	  (if ,sub-state
	      (set-sub-state ,sub-state))))
(defun get-state ()
  tamias:state)
(defvar states nil)

(define-state tamias-default-state)
(set-state tamias-default-state)

"New code, needs testing and work"

(defgeneric tamias-mouse (mouse-button mouse-state state sub-state move?)
  (:method (mouse-button mouse-state state sub-state move?)
    nil)
    ;;(print "Default mouse method"))
  )
(defmacro add-mouse (mouse-button (state sub-state mouse-state move?) &body body)
  `(defmethod tamias-mouse ((mouse-button (eql ,mouse-button))
			    (mouse-state (eql ,mouse-state))
			    (state (eql ,state))
			    (sub-state (eql ,sub-state))
			    (move? (eql ,move?)))
     ,@body))

(defgeneric tamias-key (key key-state state sub-state ctrl alt shift)
  (:method (key key-state state sub-state ctrl alt shift)
    nil)
;;    (print "Default key method."))
  )

(defmacro add-key (key (state sub-state key-state &key ctrl alt shift) &body body)
  `(defmethod tamias-key ((key (eql ,key))
			  (key-state (eql ,key-state))
			  (state (eql ,state))
			  (sub-state (eql ,sub-state))
			  (ctrl (eql ,ctrl))
			  (alt (eql ,alt))
			  (shift (eql ,shift)))
     ,@body))

(defgeneric tamias-render (state sub-state)
  (:method (state sub-state)
    nil)
  ;;    (print "default render method."))
  )
(defmacro def-render ((state sub-state) &body body)
  `(defmethod tamias-render ((state (eql ,state))
			     (sub-state (eql ,sub-state)))
     ,@body))

(defgeneric tamias-logic (state sub-state)
  (:method (state sub-state)
    nil)
;;    (print "default logic method."))
  )
(defmacro def-logic ((state sub-state) &body body)
  `(defmethod tamias-logic ((state (eql ,state))
			    (sub-state (eql ,sub-state)))
     ,@body))

(defgeneric activate-interactive-object (tamias-object key-board? key-state ctrl alt shift mouse? mouse-button mouse-state move?)
  (:method (tamias-object key-board? key-state ctrl alt shift mouse? mouse-state move?)
    nil)
  )

(defmacro activate-object (tamias-object key-board? (&optional key-state ctrl alt shift) mouse? (&optional mouse-button mouse-state move?) &body body)
  `(defmethod activate-interactive-object ((tamias-object (eql ,tamias-object))
					   (key-board? (eql ,key-board?))
					   (key-state (eql ,key-state))
					   (ctrl (eql ,ctrl))
					   (alt (eql ,alt))
					   (shift (eql ,shift))
					   (mouse? (eql ,mouse?))
					   (mouse-button (eql ,mouse-button))
					   (mouse-state (eql ,mouse-state))
					   (move? (eql ,move?)))
     ,@body))
;;The Idea of a "pass-through" is that an input is passed through the state/sub-state sub-system and is directly
;;passed through to the various components, like UI or In-game objects
;;So that when you press "a" a is passed through to the current context's input system
;;SO the "body" would look something like (current-context-input input)
;;I do want to figure out how to make methods for various structs.
;;
;;I want to have it so that each 'object' can have it's own interaction tree, rather than having to afro-engineer together a solution with the current architecture
;;The reason is fairly simple: it allows for a much more complex interaction system
;;The Downside, however, is increased processing time, ram usage, and storage space.
;;The Upside is simplified complexity, ease of development & use, encapsulation, and better organized code
;;
;;(defun add-pass-through (state sub-state pass-through-type)
;;  (case pass-through-type
;;    ('key ())
;;    ('mouse ())))


#|
(defmacro init-input (state)
  `(setf (gethash :down (state-keys ,state)) (make-hash-table)
	 (gethash :up (state-keys ,state)) (make-hash-table)
	 (gethash :down (state-mouse ,state)) (make-hash-table)
	 (gethash :up (state-mouse ,state)) (make-hash-table)))
|#

#|(defmacro add-to-state-render (function-name state)
  `(setf (state-render-list ,state) (append (state-render-list ,state) (list (quote ,function-name)))))
(defmacro define-state-render (function-name state &body body)
  `(progn (defun ,function-name ()
	    ,@body)
	  (setf (state-render-list ,state) (append (state-render-list ,state) (list (quote ,function-name))))))
(defmacro define-render-function (function-name state &body body)
  `(define-state-render ,function-name ,state ,body))


  
(defmacro add-key (key state key-state &rest function-name)
  `(setf (gethash ,key (gethash ,key-state (state-keys ,state)))
	 `,(quote ,function-name)))

(defmacro add-substate-function (function-name state sub-state)
  `(if (gethash ,sub-state (state-loops ,state))
       (append (quote ,function-name) (gethash ,sub-state (state-loops ,state)))
       (push (quote ,function-name) (gethash ,sub-state (state-loops ,state)))))

(defmacro add-loop-function (function-name state sub-state)
  `(push (quote ,function-name) (gethash ,sub-state (state-loops ,state))))
(defmacro define-loop-function (function-name state sub-state &body body)
  `(progn (defun ,function-name ()
	    ,@body)
	  (push (quote ,function-name) (gethash ,sub-state (state-loops ,state)))))

(defmacro add-mouse (mouse-button state mouse-state &rest function-name)
  `(if ,mouse-button
       (setf (gethash ,mouse-button (gethash ,mouse-state (state-mouse ,state)))
	     `,(quote ,function-name))
       (setf (gethash :move (state-mouse ,state))
	     `,(quote ,function-name))))
|#


