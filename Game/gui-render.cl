"the def-render is to render the cursor
It isn't necessary, but it'll help the user track where the cursor is and what they are clicking on

If a macro or function appears directly before a method, that macro/function is there for that method
example: (defun render.ui-spin-box ...) (defmethod ... (eql 'spin-box)...)
a generalized macro/function will be at teh top, before the rendering method for the ui-button
"

(defun render.cursor (&optional w h color)
  (render:box (round (- tamias:*mouse-x* (/ tamias:*cursor-size* 2)))
	      (round (- tamias:*mouse-y* (/ tamias:*cursor-size* 2)))
	      (or w tamias:*cursor-size*)
	      (or h tamias:*cursor-size*)
	      :color (or color tamias.colors:+white+)))

(defmacro render.ui-text (str x y w h)
  `(progn ;(render:box ,x ,y ,w ,h :color (list 0 34 55 255))
	  ;(render:box (+ ,x 2) (+ ,y 2) (- ,w 3) (- ,h 3) :color (list 0 19 32 255))
	  ;(render:box (+ ,x 4) (+ ,y 4) (- ,w 6) (- ,h 6) :color (list 0 73 118 255))
	  (render:text ,str (+ ,x 1) (+ ,y 1) :color tamias.colors:+white+)))

(defun render.ui-bg (ui-element &optional x y w h)
  (let ((x (or x (ui-element-x ui-element)))
	(y (or y (ui-element-y ui-element)))
	(w (or w (ui-element-width ui-element)))
	(h (or h (ui-element-height ui-element))))
    (if (ui-base-color ui-element)
      (render:box x y w h :color (ui-base-color ui-element))
      (render:box x y w h :color tamias.colors:+red+))))

(defun render.ui-hover (ui-element)
  (let ((x (ui-element-x ui-element))
	(y (ui-element-y ui-element))
	(w (ui-element-width ui-element))
	(h (ui-element-height ui-element)))
    (if (ui-base-color ui-element)
	(let ((color (ui-base-color ui-element)))
	  (let ((r (car color))
		(g (cadr color))
		(b (caddr color)))
	    (if (> r 127)
		(if (> r 191)
		    (setf r (- 128 (- r 128)))
		    (setf r (- r 64)))
		(if (< r 63)
		    (setf r (+ r r r)) ;;Add is faster than mult
		    (setf r (+ r r))))
	    (if (> g 127)
		(if (> g 191)
		    (setf g (- 128 (- g 128)))
		    (setf g (- g 64)))
		(if (< g 63)
		    (setf g (+ g g g)) ;;Add is faster than mult
		    (setf g (+ g g))))
	    (if (> b 127)
		(if (> b 191)
		    (setf b (- 128 (- b 128)))
		    (setf b (- b 64)))
		(if (< b 63)
		    (setf b (+ b b b)) ;;Add is faster than mult
		    (setf b (+ b b))))
	    (render:box x y w h :color (list r g b 127))))
	(let ((color tamias.colors:+pastel-pink+))
	  (render:box x y w h :color (list (car color) (cadr color) (caddr color) 127))))))
    

(defmethod render.ui (ui-element (ui-type (eql 'button)) &optional hover)
  (render.ui-bg ui-element)
  (if hover
      (render.ui-hover ui-element)))

(defmethod render.ui (ui-element (ui-type (eql 'label)) &optional hover)
  (if (not (eval (ui-label-hidden ui-element)))
      (let ((x (ui-element-x ui-element))
	    (y (ui-element-y ui-element))
	    (w (ui-element-width ui-element))
	    (h (ui-element-height ui-element))
	    (str (ui-element-label ui-element)))
	(if (not (stringp str))
	    (setf str (eval str)))
	(render.ui-bg ui-element)
	;;    (if ui-color
	;;	(render:box x y w h :color (ui-element-color ui-element)))
	(let ((line-limit (round (/ w (car tamias.string:character-size)))))
	  (if (< (length str) line-limit)
	      (render.ui-text str x y w h)
	      (let ((rope-group (tamias.string:text-chunker str line-limit)))
		(loop for text in rope-group
		      for y-acc below (length rope-group)
		      do (render.ui-text text x (+ y (* y-acc (cadr tamias.string:character-size)))
					 w h)))))
      	(if hover
	    (render.ui-hover ui-element)))))

(defmethod render.ui (ui-element (ui-type (eql 'entry)) &optional hover)
  (let ((x (ui-base-x ui-element))
	(y (ui-base-y ui-element))
	(w (ui-base-width ui-element))
	(h (ui-base-height ui-element))
	(str (text-entry-text (ui-element-entry ui-element)))
	(text-struct (ui-element-entry ui-element)))
    (render.ui-bg ui-element)
    (if hover
	(render.cursor 8 36 (list 200 200 200 255))
	;;send message to mouse sub-system to switch to text cursor, that "I" thing
	)
    (if (eq current-text-context text-struct)
	(progn (cursor-blink-check text-struct)
	       (if (text-entry-line-length text-struct)
		   (let ((y-acc 0)
			 (x-acc 0)
			 (text-group (tamias.string:text-chunker str (text-entry-line-length text-struct))))
		     (loop for txt in text-group
			   do (if (> (text-entry-position text-struct)
				     (+ y-acc (text-entry-line-length text-sturct)))
				  (incf y-acc (text-entry-line-length text-sturct))
				  (setf x-acc (- (text-entry-position text-struct)
						 y-acc))))
		     (if (not (tamias-text-cursor-hide text-struct))
			 (render:box (+ x x-acc) (+y y-acc)
				     3 (cadr tamias.string:character-size))))
		   (if (not (tamias-text-cursor-hide text-struct))
		       (render:box (+ x (* 16 (text-entry-position text-struct))) y
				   3 (cadr tamias.string:character-size))))))
    (render.ui-text str x y w h)))

(defmethod render.ui (ui-element (ui-type (eql 'spin-box)) &optional hover)
  (let ((x-max (+ (ui-element-x ui-element) (ui-element-width ui-element)))
	(y-max (+ (ui-element-y ui-element) (ui-element-height ui-element)))
	(str (write-to-string (tamias-variable-value (number-entry-value (ui-element-special ui-element))))))
    (render.ui-text str (ui-element-x ui-element) (ui-element-y ui-element)
		    (ui-element-width ui-element) (ui-element-height ui-element))
    (render.ui-bg ui-element (- x-max 16) (- y-max 16) 16 8)
    (render.ui-bg ui-element (- x-max 16) (- y-max 16) 14 6)
    (render.ui-bg ui-element (- x-max 16) (- y-max 8) 16 8)
    (render.ui-bg ui-element (- x-max 16) (- y-max 8) 14 6)
    (render:text "+1" (- x-max 14) (- y-max 14) :width 14 :height 6)
    (render:text "-1" (- x-max 14) (- y-max 7) :width 14 :height 6)))

(defmethod render.ui (ui-element (ui-type (eql 'frame)) &optional hover)
  "loop through each element in the frame and render it."
  )

(defun render.menu (ui-menu x-offset y-offset)
  (render:box x-offset y-offset
	      (ui-menu-width ui-menu) (* (length (ui-menu-ids ui-menu)) (cadr tamias.string:character-size))
	      :color (ui-menu-color ui-menu)))

(defun render.menu-item (ui-element x-offset y-offset &key width)
  (render:box x-offset y-offset
	      (or width (ui-menu-item-width ui-element))
	      (cadr tamias.string:character-size)
	      :color (ui-menu-item-color ui-element))
  (render:text (ui-menu-item-label ui-element) x-offset y-offset)
  (let ((ui-menu (ui-menu-item-menu ui-element)))
    (if ui-menu
	(if (ui-menu-active? ui-menu)
	    (let ((current-x-offset (+ x-offset (ui-menu-item-width ui-element)))
		  (current-y-offset y-offset)
		  (ui-menu (ui-menu-active-item ui-menu)))
	      (render.menu ui-menu x-offset y-offset)
	      (loop for ui-item in (ui-menu-ids ui-menu)
		 do (render.menu-item current-x-offset current-y-offset
				      :width (ui-menu-item-drop-down-width ui-element))
		   (incf current-y-offset (cadr tamias.string:character-size))))))))

(defun render.menu-bar-item (ui-element x-offset y-offset &key width active)
  (render:box x-offset y-offset
	      (or width (ui-menu-item-width ui-element))
	      (cadr tamias.string:character-size)
	      :color (ui-menu-item-color ui-element))
  (if active
      (render:box x-offset y-offset
		  (or width (ui-menu-item-width ui-element))
		  (cadr tamias.string:character-size)
		  :color tamias.colors:+pastel-pink+))
  (render:text (ui-menu-item-label ui-element) x-offset y-offset))

(defmethod render.ui (ui-element (ui-type (eql 'menu-bar)) &optional hover)
  (render:box (ui-element-x ui-element) (ui-element-y ui-element)
	      (ui-element-width ui-element) (ui-element-height ui-element)
	      :color (ui-element-color ui-element))
  (let ((current-x-offset 0)
	(current-y-offset 0)
	(active-item (ui-menu-bar-active-item ui-element)))
    (loop for item in (ui-menu-bar-items ui-element) ;;with each new item that's dealt with, increase the current-x-offset
       do (if (not active-item)
	      (render.menu-bar-item item current-x-offset current-y-offset)
	      (progn (render.menu-bar-item item current-x-offset current-y-offset :active t)
		     (loop for ui-item in (ui-menu-bar-item-ids (gehash active-item (ui-menu-bar-items ui-element)))
			do (incf current-y-offset (cadr tamias.string:character-size))
			  (render.menu ui-item current-x-offset current-y-offset
				       :width (ui-menu-bar-item-drop-down-width item))
			  (incf current-x-offset (ui-menu-bar-item-width item))
			  (setf current-y-offset 0)))))))

(defmethod render.ui (ui-element (ui-type (eql 'menu-item)) &optional hover)
  )
		      

(defun render.menu-bar (state sub-state)
  (let ((menu-bar (get-menu-bar state sub-state)))
    (if menu-bar
	(render.ui menu-bar 'menu-bar))))

