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

;;5/16/2019
;;actually, having the quadrant be a part of the entity might be a good idea
;;but it could also be terrible
;;No, that defeats the entire point of quad trees
;;Quadrants 1-4 has a list of everything in it
;;These Quadrants will "always" have objects in them, even if the objects are in a subdivision
;;I.e. If Q1 has been split 4 times, Then Entity A will 'reside' in Q1, Q5, Q9, and Q13 (if Q2/3/4 haven't been split)
;;When a child node is destroyed, it doesn't affect the nodes above it

;;So, Idea:
#|

If Q1 hasn't been split, then it tests collisions of every object in it's field
If Q1 has been split, then the child nodes are tested, if they themselves haven't split
If they have, then it keeps going down the quadrants child nodes
When a quadrant's object list length falls below X, then it get's rid of the child
When a quadrant's object list length goes above X, then it splits

|#

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
  (height 0)
  (objects '()))
;;nodes are used to check the objects x and y
;;if the object lies in multiple nodes, then it'll 
(defvar tamias-quad-tree nil)

(defmacro init-quad-tree (&key obects)
  `(setf tamias-quad-tree (make-quad-tree :width tamias:screen-width
					  :height tamias:screen-height
					  :objects ,objects
					  :nodes (list (make-qt-node :x 0
								     :y 0
								     :width (round (/ tamias:screen-width 2))
								     :height (round (/ tamias:screen-height 2)))
						       (make-qt-node :x (round (/ tamias:screen-width 2))
								     :y 0
								     :width (round (/ tamias:screen-width 2))
								     :height (round (/ tamias:screen-height 2)))
						       (make-qt-node :x 0
								     :y (round (/ tamias:screen-height 2))
								     :width (round (/ tamias:screen-width 2))
								     :height (round (/ tamias:screen-height 2)))
						       (make-qt-node :x (round (/ tamias:screen-width 2))
								     :y (round (/ tamias:screen-height 2))
								     :width (round (/ tamias:screen-width 2))
								     :height (round (/ tamias:screen-height 2)))))))
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
	 (q1 nil)
	 (q2 nil)
	 (q3 nil)
	 (q4 nil)
	 (objs nil))
     (loop for object in (quad-tree-objects ,parent-quad-tree)
	do (if (and (< (t-object-x object) (qt-node-width parent-node))
		    (< (t-object-y object) (qt-node-height parent-node)))
	       (push object objs)))
     (loop for object in objs
	do (cond ((and (< (t-object-x object) (/ (qt-node-width parent-node) 2))
		       (< (t-object-y object) (/ (qt-node-height parent-node) 2)))
		  (push object q1))
		 ((and (> (t-object-x object) (/ (qt-node-width parent-node) 2))
		       (< (t-object-y object) (/ (qt-node-height parent-node) 2)))
		  (push object q2))
		 ((and (< (t-object-x object) (/ (qt-node-width parent-node) 2))
		       (> (t-object-y object) (/ (qt-node-height parent-node) 2)))
		  (push object q3))
		 ((and (> (t-object-x object) (/ (qt-node-width parent-node) 2))
		       (> (t-object-y object) (/ (qt-node-height parent-node) 2)))
		  (push object q4))))
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
								    :height (round (/ (qt-node-height parent-node) 2))
								    :objects q1)
						      (make-qt-node :x (round (/ (qt-node-width parent-node) 2))
								    :y (qt-node-y parent-node)
								    :width (round (/ (qt-node-width parent-node) 2))
								    :height (round (/ (qt-node-height parent-node) 2))
								    :objects q2)
						      (make-qt-node :x (qt-node-x parent-node)
								    :y (round (/ (qt-node-height parent-node) 2))
								    :width (round (/ (qt-node-width parent-node) 2))
								    :height (round (/ (qt-node-height parent-node) 2))
								    :objects q3)
						      (make-qt-node :x (round (/ (qt-node-width parent-node) 2))
								    :y (round (/ (qt-node-height parent-node) 2))
								    :width (round (/ (qt-node-width parent-node) 2))
								    :height (round (/ (qt-node-height parent-node) 2))
								    :objects q4))))
		   (quad-tree-children ,parent-quad-tree)))
     (loop for obj in objs
	do (setf (quad-tree-objects ,parent-quad-tree) (remove obj (quad-tree-objects ,parent-quad-tree))))))
(defmacro delete-quad-tree (quad-tree parent)
  `(let ((objs (quad-tree-objects ,quad-tree)))
     (push objs (quad-tree-objects ,parent))
     (setf ,quad-tree nil)))

(defun handle-quad-tree ()
  ;;Check collisions. Have a flag in the entity structure (collided) that on collision with another object is set to "entity ID" (position in Entities or ID in ECS)
  ;;There'll need to be a resolution system set up
  )
  
(defun update-quad-tree (&key (quad-tree tamias-quad-tree) parent)
  (if (quad-tree-children quad-tree)
      (loop for child in (quad-tree-children quad-tree)
	 do (handle-quad-tree :quad-tree child :parent quad-tree)))
  (if (> (quad-level quad-tree) 0)
      (if (< (length (quad-tree-objects quad-tree)) 8)
	  (delete-quad-tree quad-tree parent)))
  (if (and (>= (length (quad-tree-objects quad-tree)) 16)
	   (not (quad-tree-children quad-tree)))
      (let ((q1 (length (qt-node-objects (car (quad-tree-nodes quad-tree)))))
	    (q2 (length (qt-node-objects (cadr (quad-tree-nodes quad-tree)))))
	    (q3 (length (qt-node-objects (caddr (quad-tree-nodes quad-tree)))))
	    (q4 (length (qt-node-objects (cadddr (quad-tree-nodes quad-tree))))))
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
	      :color +white+
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
