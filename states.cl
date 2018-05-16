(defstruct state
  (loops (make-hash-table))
  (keys (make-hash-table))
  (mouse (make-hash-table))
  (transition t)
  (render-list '()))

(defmacro start-input (state)
  `(setf (gethash :down (state-keys ,state)) (make-hash-table)
	 (gethash :up (state-keys ,state)) (make-hash-table)
	 (gethash :down (state-mouse ,state)) (make-hash-table)
	 (gethash :up (state-mouse ,state)) (make-hash-table)))
(defmacro define-state (state)
  `(progn (defvar ,state (make-state))
	  (start-input ,state)))


(defmacro add-to-state-render (function-name state)
  `(setf (state-render-list ,state) (append (state-render-list ,state) (list (quote ,function-name)))))
(defmacro define-state-render (function-name state &body body)
  `(progn (defun ,function-name ()
	    ,@body)
	  (setf (state-render-list ,state) (append (state-render-list ,state) (list (quote ,function-name))))))

(defmacro add-key (key state key-state &rest function-name)
  `(setf (gethash ,key (gethash ,key-state (state-keys ,state)))
	 `,(quote ,function-name)))

(defmacro add-substate-function (function-name state sub-state)
  `(if (gethash ,sub-state (state-loops ,state))
       (append (quote ,function-name) (gethash ,sub-state (state-loops ,state))))
       (push (quote ,function-name) (gethash ,sub-state (state-loops ,state))))

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
