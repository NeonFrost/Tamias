;;fuuuuck
;;basically shit
;;holy fuck my head
;;please, god, end my suffering right now
;;quad-trees are a test from the eternal god
;;the eternal light, the infinite tree, the fires of procyon

;;basically, I'm worried if how I implement quad-trees will be efficient and performant
;;it is for vidya, so fuck me silly daddy
;;I'm not sure how to go about implementing it fully
;;Like, should I make a collision function for quad-trees that uses the nodes to do shit?
;;basically, loop through the objects, assign them a quadrant value or have a quadrant var (q1 q2 q3 q4)
;;then use that to...do...stuff...
#|
So essentially
(let ((q1 nil) (q2 nil) (q3 nil) (q4 nil))
  (loop for object in objects

|#
(defstruct quad-tree
  (x 0)
  (y 0)
  (width 1360) ;;
  (height 720) ;;
  (objects '())
  children
  nodes
  (level 0))
(defstruct qt-node
  (x 0)
  (y 0)
  (width 0)
  (height 0))
;;nodes are used to check the objects x and y
;;if the object lies in multiple nodes, then it'll 
(defvar tamias-quad-tree nil)

(defmacro init-quad-tree (&key obects)
  `(setf tamias-quad-tree (make-quad-tree :width *screen-width*
					  :height *screen-height*
					  :objects ,objects
					  :nodes (list (make-qt-node :x 0
								     :y 0
								     :width (round (/ *screen-width* 2))
								     :height (round (/ *screen-height* 2)))
						       (make-qt-node :x (round (/ *screen-width* 2))
								     :y 0
								     :width (round (/ *screen-width* 2))
								     :height (round (/ *screen-height* 2)))
						       (make-qt-node :x 0
								     :y (round (/ *screen-height* 2))
								     :width (round (/ *screen-width* 2))
								     :height (round (/ *screen-height* 2)))
						       (make-qt-node :x (round (/ *screen-width* 2))
								     :y (round (/ *screen-height* 2))
								     :width (round (/ *screen-width* 2))
								     :height (round (/ *screen-height* 2)))))))
(defmacro collect-child-objects (quad-tree)
  `(let ((objs nil))
     (if (quad-tree-children ,quad-tree)
	 (push (collect-child-objects ,quad-tree) objs))
     objs))
(defmacro reset-quad-tree ()
  `(let ((objects nil))
     (if (quad-tree-children tamias-quad-tree)
	 (loop for child in (quad-tree-children tamias-quad-tree)
	   do (push (collect-child-objects tamias-quad-tree) objects)))
     (init-quad-tree :objects objects)))
;;What will need to happen is that if there are children, then it uses the children

(defmacro create-quad-tree (parent-quad-tree parent-node)
  `(let ((parent-node (nth ,parent-node (quad-tree-nodes ,parent-quad-tree)))
	 (objs nil))
     (loop for obejct in (quad-tree-objects ,parent-quad-tree)
	do (if (and (< (t-object-x object) (qt-node-width parent-node))
		    (< (t-object-y object) (qt-node-height parent-node)))
	       (push object objs)))
     (setf (quad-tree-children ,parent-quad-tree)
	   (append (list (make-quad-tree :x (qt-node-x parent-node)
					 :y (qt-node-y parent-node)
					 :width (qt-node-width parent-node)
					 :height (qt-node-height parent-node)
					 :objects objs
					 :level (1+ (quad-tree-level ,parent-quad-tree))
					 :nodes (list (make-qt-node :x (qt-node-x parent-node)
								    :y (qt-node-y parent-node)
								    :width (round (/ (qt-node-width parent-node) 2))
								    :height (round (/ (qt-node-height parent-node) 2)))
						      (make-qt-node :x (round (/ (qt-node-width parent-node) 2))
								    :y (qt-node-y parent-node)
								    :width (round (/ (qt-node-width parent-node) 2))
								    :height (round (/ (qt-node-height parent-node) 2)))
						      (make-qt-node :x (qt-node-x parent-node)
								    :y (round (/ (qt-node-height parent-node) 2))
								    :width (round (/ (qt-node-width parent-node) 2))
								    :height (round (/ (qt-node-height parent-node) 2)))
						      (make-qt-node :x (round (/ (qt-node-width parent-node) 2))
								    :y (round (/ (qt-node-height parent-node) 2))
								    :width (round (/ (qt-node-width parent-node) 2))
								    :height (round (/ (qt-node-height parent-node) 2)))))) (quad-tree-children ,parent-quad-tree)))
     (loop for obj in objs
	do (setf (quad-tree-objects ,parent-quad-tree) (remove obj (quad-tree-objects ,parent-quad-tree))))))
(defmacro delete-quad-tree (quad-tree parent)
  `(let ((objs (quad-tree-objects ,quad-tree)))
     (push objs (quad-tree-objects ,parent))
     (setf ,quad-tree nil)))
  
(defun handle-quad-tree (&key (quad-tree tamias-quad-tree) parent)
  (if (quad-tree-children quad-tree)
      (loop for child in (quad-tree-children quad-tree)
	 do (handle-quad-tree :quad-tree child :parent quad-tree)))
  (if (> (quad-level quad-tree) 0)
      (if (< (length (quad-tree-objects quad-tree)) 8)
	  (delete-quad-tree quad-tree parent)))
  (if (>= (length (quad-tree-objects quad-tree)) 16)
      (let ((q1 nil) (q2 nil) (q3 nil) (q4 nil))
	(loop for object in (quad-tree-objects quad-tree)
	   do (cond ((and (< (t-object-x object) (round (/ (quad-tree-width quad-tree) 2)))
			  (< (t-object-y object) (round (/ (quad-tree-height quad-tree) 2))))
		     (push object q1))
		    ((and (>= (t-object-x object) (round (/ (quad-tree-width quad-tree) 2)))
			  (< (t-object-y object) (round (/ (quad-tree-height quad-tree) 2))))
		     (push object q2))
		    ((and (< (t-object-x object) (round (/ (quad-tree-width quad-tree) 2)))
			  (>= (t-object-y object) (round (/ (quad-tree-height quad-tree) 2))))
		     (push object q3))
		    ((and (>= (t-object-x object) (round (/ (quad-tree-width quad-tree) 2)))
			  (>= (t-object-y object) (round (/ (quad-tree-height quad-tree) 2))))
		     (push object q4))))
	(if (>= q1 8)
	    (create-quad-tree quad-tree 0))
	(if (>= q2 8)
	    (create-quad-tree quad-tree 1))
	(if (>= q3 8)
	    (create-quad-tree quad-tree 2))
	(if (>= q4 8)
	    (create-quad-tree quad-tree 3)))))

(defun print-quad-tree (quad-tree)
  (princ (combine-strings "QUAD-TREE X       :" (write-to-string (quad-tree-x quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE Y       :" (write-to-string (quad-tree-y quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE WIDTH   :" (write-to-string (quad-tree-width quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE HEIGHT  :" (write-to-string (quad-tree-height quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE LEVEL   :" (write-to-string (quad-tree-level quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE OBJECTS :" (write-to-string (quad-tree-objects quad-tree)))) (fresh-line)
  (princ "------------------------") (fresh-line)
  (if (quad-tree-children quad-tree)
      (loop for child in (quad-tree-children quad-tree)
	 do (print-quad-tree child)
	   #|(princ (combine-string "QUAD-TREE X      :" (write-to-string (quad-tree-x quad-tree)))) (fresh-line)
	   (princ (combine-string "QUAD-TREE Y      :" (write-to-string (quad-tree-y quad-tree)))) (fresh-line)
	   (princ (combine-string "QUAD-TREE WIDTH  :" (write-to-string (quad-tree-width quad-tree)))) (fresh-line)
	   (princ (combine-string "QUAD-TREE HEIGHT :" (write-to-string (quad-tree-height quad-tree)))) (fresh-line)       |#
	   )
      ))

(defun draw-qt-node (node)
  (render-box (qt-node-x node) (qt-node-y node)
	      (qt-node-width node) (qt-node-height node)
	      :filled nil))

(defun draw-qt-child (child)
  (loop for node in (quad-tree-nodes child)
     do (draw-qt-node node))
  (if (quad-tree-children child)
      (loop for nu-child in (quad-tree-children child)
	   do (draw-qt-child nu-child))))

(defun draw-quad-tree ()
  (loop for node in (quad-tree-nodes tamias-quad-tree)
     do (draw-qt-node node))
  (if (quad-tree-children tamias-quad-tree)
      (loop for child in (quad-tree-children tamias-quad-tree)
	   do (draw-qt-child child))))

  #|

  [] -> [[] -- [] -- [] -- []] \
  .----------------------------- 
  |
  V
  [[[] -- [] -- [] -- []] :<>: 
  [[] -- [] -- [] -- []] :<>: 
  [[] -- [] -- [] -- []] :<>: 
  [[] -- [] -- [] -- []]]
  
  |#
