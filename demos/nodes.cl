(defstruct (node (:include t-object))
  title
  lines
  (fill +cream+)
  border)
(defstruct node-line
  x
  y
  x2
  y2)

(defvar *nodes* '())
(defvar current-node nil)

(defmacro define-node (&key (x 0) (y 0) (width 128) (height 192) (title "New Node"))
  `(push (make-node :x ,x :y ,y :width ,width :height ,height :title ,title) *nodes*))
(defun create-node (title)
  (define-node :title title))

(define-state *node-editor*)
(setf state '*node-editor*)

(defun select-node (x y)
  (loop for node in *nodes*
     do (if (and (and (>= x (node-x node))
		      (<= x (+ (node-x node) (node-width node))))
		 (and (>= y (node-y node))
		      (<= y (+ (node-y node) (node-height node)))))
	    (progn (setf current-node (position node *nodes*)
			 current-text-context (node-title node)
			 *current-text-position* (length (node-title node)))
		   (return t)))))

(defun edit-node-title ()
  (if *text-input-state*
      (setf (node-title (nth current-node *nodes*)) current-text-context)))
(add-loop-function edit-node-title *node-editor* 'top)

(defun move-node ()
  (progn (decf (node-x (nth current-node *nodes*)) (- canvas-x-relation *mouse-x*))
	 (decf (node-y (nth current-node *nodes*)) (- canvas-y-relation *mouse-y*))
	 (setf canvas-x-relation *mouse-x*
	       canvas-y-relation *mouse-y*))
  (let ((node (nth current-node *nodes*)))
    (loop for node-line in (node-lines (nth current-node *nodes*))
       do (setf (node-line-x node-line) (round (+ (node-x node) (/ (node-width node) 2)))
		(node-line-y node-line) (round (+ (node-y node) (/ (node-height node) 2)))
		))))

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
	      (node-height node)
	      :color (node-fill node))
  (render-box (- (node-x node) 2)
	      (+ (node-y node) 2)
	      (+ (node-width node) 4)
	      (- (node-height node) 4)
	      :color (node-fill node))
  (render-box (+ (node-x node) 2)
	      (- (node-y node) 2)
	      (- (node-width node) 4)
	      (+ (node-height node) 4)
	      :color (node-fill node))
  (if (> (length (node-title node)) 0)
      (render-string (node-title node)
		     (1+ (node-x node))
		     (1+ (node-y node))
		     :width (- (node-width node) 2)
		     :height (- (node-height node) 2)
		     :color +dark-steel-blue+)))

(defvar canvas-x 0)
(defvar canvas-y 0)

(defun move-canvas ()
  (loop for node in *nodes*
     do (decf (node-x node) (- canvas-x-relation *mouse-x*))
       (decf (node-y node) (- canvas-y-relation *mouse-y*))
      (loop for node-line in (node-lines node)
	 do (setf (node-line-x node-line) (round (+ (node-x node) (/ (node-width node) 2)))
		  (node-line-y node-line) (round (+ (node-y node) (/ (node-height node) 2)))))))

(defun render-node-canvas ()
  (loop for y below *screen-height* by (round (/ *screen-height* 32))
     do (draw-line 0 y
		   *screen-width* y
		   :color +dark-steel-blue+)
       (draw-line 0 (1+ y)
		   *screen-width* (1+ y)
		   :color +dark-steel-blue+))
  (loop for x below *screen-width* by (round (/ *screen-width* 32))
     do (draw-line x 0
		   x *screen-height*
		   :color +dark-steel-blue+)
       (draw-line (1+ x) 0
		   (1+ x) *screen-height*
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
(defvar canvas-x-relation 0)
(defvar canvas-y-relation 0)
(add-mouse :button-left *node-editor* :down (if (select-node *mouse-x* *mouse-y*)
						(if (not (shift-t))
						    (progn (setf *text-input-state* t
								 canvas-x-relation *mouse-x*
								 canvas-y-relation *mouse-y*
								 *current-text-position* (length current-text-context))
							   (sdl2:start-text-input))
						    (if (not (control-t))
							(create-node-line (nth current-node *nodes*))
							(setf canvas-x-relation *mouse-x*
							      canvas-y-relation *mouse-y*)))
						(progn (setf *text-input-state* nil
							     canvas-x-relation 0
							     canvas-y-relation 0
							     current-node nil)
						       (sdl2:stop-text-input))))
(add-mouse :button-left *node-editor* :up (setf canvas-x-relation 0
						canvas-y-relation 0))
(add-mouse :button-middle *node-editor* :down (if (not *text-input-state*)
						  (progn (if (or (eq canvas-x-relation 0)
								 (eq canvas-y-relation 0))
							     (setf canvas-x-relation *mouse-x*
								   canvas-y-relation *mouse-y*))
							 (move-canvas))))
(add-mouse :button-middle *node-editor* :up (setf canvas-x 0
						  canvas-y 0
						  canvas-x-relation 0
						  canvas-y-relation 0))
(add-mouse nil *node-editor* :move (if (and current-node
					    (not (or (eq canvas-x-relation 0)
						     (eq canvas-y-relation 0))))
				       (move-node)
				       (if current-node
					   (if (control-t)
					       (if (node-lines (nth current-node *nodes*))
						   (progn (edit-node-line (nth 0 (node-lines (nth current-node *nodes*)))
									  (- (node-line-x (nth 0 (node-lines (nth current-node *nodes*))))
									     (- (node-line-x (nth 0 (node-lines (nth current-node *nodes*)))) *mouse-x*))
									  (- (node-line-y (nth 0 (node-lines (nth current-node *nodes*))))
									     (- (node-line-y (nth 0 (node-lines (nth current-node *nodes*)))) *mouse-y*))))))
					   (if (not (or (eq canvas-x-relation 0)
							(eq canvas-y-relation 0)
							*text-input-state*))
					       (progn (move-canvas)
						      (setf canvas-x-relation *mouse-x*
							    canvas-y-relation *mouse-y*))))))
					       
(add-key :scancode-a *node-editor* :down (if (not *text-input-state*)
					     (define-node :x *mouse-x*
					       :y *mouse-y*)))
