(defstruct quad-tree
  (x 0)
  (y 0)
  (width 1360) ;;
  (height 720) ;;
  (objects '())
  children
  nodes
  (tree-level 0))
(defstruct qt-node
  (x 0)
  (y 0)
  (width 0)
  (height 0))

(defmacro init-quad-tree (quad-tree)
  `(defvar ,quad-tree (make-quad-tree :width *screen-width*
				      :height *screen-height*
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
;;What will need to happen is that if there are children, then it uses the children

(defmacro create-quad-tree (parent-quad-tree parent-node tree-level)
  `(let ((parent-node (nth ,parent-node (quad-tree-nodes ,parent-quad-tree))))
     (setf (quad-tree-children ,parent-quad-tree)
	   (append (list (make-quad-tree :x (qt-node-x parent-node)
					 :y (qt-node-y parent-node)
					 :width (qt-node-width parent-node)
					 :height (qt-node-height parent-node)
				       :tree-level ,tree-level
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
								  :height (round (/ (qt-node-height parent-node) 2)))))) (quad-tree-children ,parent-quad-tree)))))

(defun print-quad-tree (quad-tree)
  (princ (combine-strings "QUAD-TREE X      :" (write-to-string (quad-tree-x quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE Y      :" (write-to-string (quad-tree-y quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE WIDTH  :" (write-to-string (quad-tree-width quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE HEIGHT :" (write-to-string (quad-tree-height quad-tree)))) (fresh-line)
  (princ (combine-strings "QUAD-TREE LEVEL  :" (write-to-string (quad-tree-tree-level quad-tree)))) (fresh-line)
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

(defmacro expand-quad-tree (quad-tree)
  "Adds a new node to quad-tree."
  `(push
    )

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
