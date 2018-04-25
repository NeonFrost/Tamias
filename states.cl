#|(defstruct state
  (loops (list '(:top '())))
  (keys (list '(:down nil)
	      '(:up nil)))
  (mouse (list '(:down nil)
	       '(:up nil)
	       '(:move nil)))
  (transition t)
(render-list '()))|#
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
	 (gethash :up (state-mouse ,state)) (make-hash-table)
	 (gethash :move (state-mouse ,state)) (make-hash-table)))
(defmacro define-state (state)
  `(progn (defvar ,state (make-state))
	  (start-input ,state)))

#|
make-hashtable
(gethash key table)
(gethash scancode (gethash key-state (state-keys state)))
|#

(defmacro add-to-state-render (function-name state)
  `(setf (state-render-list ,state) (append (state-render-list ,state) (list (quote ,function-name)))))

(defmacro add-key (key state key-state &rest function-name)
  `(setf (gethash ,key (gethash ,key-state (state-keys ,state)))
	 `,(quote ,function-name)))

(defmacro add-substate-function (function-name state sub-state)
  `(push (quote ,function-name) (gethash ,sub-state (state-loops ,state))))

(defmacro add-loop-function (function-name state sub-state)
  `(push (quote ,function-name) (gethash ,sub-state (state-loops ,state))))
#|


(defmacro add-substate-function (function-name state sub-state)
  `(setf (gethash ,sub-state (state-loops ,state)) (append (list (gethash ,sub-state (state-loops ,state))) (list (quote ,function-name)))))
(defmacro add-loop-function (function-name state sub-state)
  `(setf (gethash ,sub-state (state-loops ,state)) (append (list (gethash ,sub-state (state-loops ,state))) (list (quote ,function-name)))))
|#
(defmacro add-mouse (mouse-button state mouse-state &rest function-name)
  `(if ,mouse-button
       (setf (gethash ,mouse-button (gethash ,mouse-state (state-mouse ,state)))
	     `,(quote ,function-name))
;;	     (append (gethash ,mouse-button (gethash ,mouse-state (state-mouse ,state)))
;;		     (list (quote ,function-name))))
       (setf (gethash :move (state-mouse ,state))
	     `,(quote ,function-name))))
;;	     (append (gethash :move (state-mouse ,state))
;;		     (list (quote ,function-name))))))


#|
(defmacro add-substate (to-state sub-state)
  `(push (list ,sub-state '()) (state-loops ,to-state)))
(defmacro add-loop (to-state loop)
  `(push (list ,loop '()) (state-loops ,to-state)))
(defmacro add-to-state-render (function-name to-state)
  `(setf (state-render-list ,to-state) (append (state-render-list ,to-state) (list (quote ,function-name)))))
(defmacro add-key (key to-state key-state &rest function-name)
  `(setf (cadr (assoc ,key-state (state-keys ,to-state))) (append (cadr (assoc ,key-state (state-keys ,to-state))) (list ,key `,(quote ,function-name)))))
(defmacro add-substate-function (function-name to-state sub-state)
  `(push (quote ,function-name) (cadr (assoc ,sub-state (state-loops ,to-state)))))
(defmacro add-loop-function (function-name to-state sub-state)
  `(push (quote ,function-name) (cadr (assoc ,sub-state (state-loops ,to-state)))))
(defmacro add-mouse (mouse-button to-state mouse-state function-name)
  `(if ,mouse-button
       (setf (cadr (assoc ,mouse-state (state-mouse ,to-state))) (append (cadr (assoc ,mouse-state (state-mouse ,to-state))) (list ,mouse-button (quote ,function-name))))
       (setf (cadr (assoc :move (state-mouse ,to-state))) (append (cadr (assoc :move (state-mouse ,to-state))) (list (quote ,function-name))))))
|#
