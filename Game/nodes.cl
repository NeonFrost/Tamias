(defstruct (node (:include object))
  title
  lines
  (fill +cream+)
  border)
(defstruct node-line
  x
  y
  x2
  y2)

(defmacro define-node (&key (x 0) (y 0) (width 64) (height 96) (title "New Node"))
  `(push (make-node :x ,x :y ,y :width ,width :height ,height :title ,title) *nodes*))
(defun create-node (title)
  (define-node :title title))

(define-state *node-editor*)
;;(defvar *node-editor* (make-state))
;;(start-input *node-editor*)
(setf state '*node-editor*)
(defvar *nodes* '())
(defvar current-node nil)

(defun select-node (x y)
  (loop for node in *nodes*
     do (if (and (>= x (node-x node))
		 (<= x (+ (node-x node) (node-width node)))
		 (>= y (node-y node))
		 (<= y (+ (node-y node) (node-height node))))
	    (progn (setf current-node (position node *nodes*)
			 current-text-context (node-title node)
			 *current-text-position* (length (node-title node)))
		   t))))
(defun edit-node-title ()
  (setf (node-title (nth current-node nodes)) current-text-context))

(defun move-node (node x y)
  (setf (node-x node) x
	(node-y node) y)
  (loop for node-line in (node-lines node)
     do (setf (node-line-x node-line) (round (+ (node-x node) (/ (node-width node) 2)))
	      (node-line-y node-line) (round (+ (node-y node) (/ (node-height node) 2)))
	      )))

(defun create-node-line (parent-node)
  (let ((x (round (+ (node-x parent-node) (/ (node-width parent-node) 2))))
	(y (round (+ (node-y parent-node) (/ (node-height parent-node) 2)))))
    (push (make-node-line :x x :y y :x2 x :y2 y) (node-lines parent-node))))

(defun edit-node-line (node-line new-x2 new-y2)
  (setf (node-line-x2 node-line) new-x2
	(node-line-y2 node-line) new-y2))

(defun render-node (node)
  (render-box (node-x node)
	      (node-y node)
	      (node-width node)
	      (node-height)
	      :color (node-fill node))
  (render-box (- (node-x node) 2)
	      (+ (node-y node) 2)
	      (+ (node-width node) 2)
	      (- (node-height node) 2)
	      :color (node-fill node))
  (render-box (+ (node-x node) 2)
	      (- (node-y node) 2)
	      (- (node-width node) 2)
	      (+ (node-height node) 2)
	      :color (node-fill node))
  (render-string (1+ (node-x node))
		 (1+ (node-y node))
		 (- (node-width node) 2)
		 (- (node-height node) 2)
		 (node-title node)))

(defvar canvas-x 0)
(defvar canvas-y 0)

(defun move-canvas ()
  (loop for node in *nodes*
     do (incf `(node-x ,node) canvas-x)
       (incf `(node-y ,node) canvas-y)
       (loop for node-line in (node-lines node)
	  do (incf `(node-line-x ,node-line) canvas-x)
	    (incf `(node-line-y ,node-line) canvas-y))))

(defun render-node-canvas ()
  (loop for y below *screen-height* by (round (/ *screen-height* 16))
     do (draw-line 0 y
		   *screen-width* y
		   :color +dark-steel-blue+))
  (loop for x below *screen-width* by (round (/ *screen-width* 16))
     do (draw-line x 0
		   x *screen-height*
		   :color +dark-steel-blue+))
  (loop for node in *nodes*
     do (render-node node)
       (if (node-lines node)
	   (loop for node-line in (node-lines node)
	      do (draw-line (node-line-x node-line)
			    (node-line-y node-line)
			    (node-line-x2 node-line)
			    (node-line-y2 node-line)
			    :color +pastel-red+)))))  
(add-to-state-render render-node-canvas *node-editor*)
;;on left-mouse click, (select-node x y) and start-text-input
;;if not, then setf current-node nil and stop-text-input
(add-mouse :button-left *node-editor* :down (if (select-node *mouse-x* *mouse-y*)
						(sdl2:start-text-input)
						(sdl2:stop-text-input)))
(add-mouse :button-middle *node-editor* :down (if (not *text-input-state*)
						  (progn (setf canvas-x *mouse-velocity-x*
							       canvas-y *mouse-velocity-y*)
							 (move-canvas))))
