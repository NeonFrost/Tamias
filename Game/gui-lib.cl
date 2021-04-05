#|
Big note, unsure if exists already elsewhere:
Development of GUI boils down to, evuentally, creating a window manager inside an application
Yes, you end up building some kind of implementation of i3, xfce, stumpwm, or etc. inside of the GUI lib
|#

(defstruct ui-manager
  collection
  menu-bar
  current-hover
  current-active)

(defstruct number-entry
  (value (make-tamias-value :type 'int :value 0))
  maximum
  minimum)

(defstruct ui-base
  type
  ID
  x
  x-equation
  y
  y-equation
  width
  width-equation
  height
  height-equation
  color
  image
  (input (make-hash-table)))

(defstruct (ui-element (:include ui-base))
  action ;;stuff to do when element is clicked or otherwise activated, like (emit-message "button clicked!")
  special ;;generic special (struct) slot, used for anything that's not an entry struct
  label
  entry
  tool-tip)

(defstruct (scroll-bar (:include ui-element
				 (action 'scroll)))
  (incrementor 1) ;;how much the window goes up or down when the up/down arrows are clicked
  (max 1)
  (current 0)
  (orientation 0)) ;;0 - right (default), 1 - left

(defstruct (ui-window (:include ui-base
				(type 'window)))
  (items (make-hash-table))
  ids
  state
  sub-state)

(defstruct (ui-frame (:include ui-base
			       (type 'frame)
			       (x 0)
			       (x-equation 0)
			       (y 0)
			       (y-equation 0)
			       (width tamias:screen-width)
			       (width-equation tamias:screen-width)
			       (height tamias:screen-height)
			       (height-equation tamias:screen-height)))
  items
  ids)
"Adding an element to a ui-frame does not bolt down it's x/y position. Rather, the x/y postion is used in conjunction with the x/y position of the ui-frame
the x/y position of an element in a frame is in relation to that frame
So, a simple example: Frame1->Frame2->Element1 . El-1 would be augmented by F2's x/y position. F2 would be augmented by F1.
"
  

;;WHile I recognize having everything in one struct isn't the best idea, mainly spatial concerns, I would rather have 10 megabytes of wasted space than have to deal with RSI

(defstruct (text-entry (:include tamias-text)))

(defstruct (ui-cursor (:include ui-element))
  (timer 0)
  (max-time 30)
  blinking?)

(defstruct (ui-text (:include ui-element
			     (type 'entry)
			     (entry (make-text-entry))))
  (cursor (make-ui-cursor)))

(defstruct (ui-label (:include ui-element
			       (type 'label)
			       (label "Label")))
  hidden)
  

(defstruct (ui-button (:include ui-element
				(type 'button)
				(x 0)
				(y 0)
				(x-equation 0)
				(y-equation 0)
				(width 32)
				(width-equation 32)
				(height 32)
				(height-equation 32)
				(color '(255 255 255 255))))
  (button-state 0) ;;0 = released, 1 = pressed
  )

(defstruct (ui-menu (:include ui-base
			      (type 'menu)
			      ))
  active-item ;;active? is a symbol
  (items (make-hash-table)) ;;hash table of ui-menu-item(s)
  ids)

(defstruct (ui-menu-item (:include ui-element
				   (type 'menu-item)
				   (x 0)
				   (y 0)))
  menu ;;is a ui-menu
  )

;;ui-menu-item-action will be a macro that is just (ui-element-special ui-menu-item)

(defstruct (ui-menu-bar (:include ui-element
				  (type 'menu-bar)
				  (x 0)
				  (x-equation 0)
				  (y 0)
				  (y-equation 0)
				  (width tamias:screen-width)
				  (width-equation 'tamias:screen-width)
				  (height (cadr tamias.string:character-size))
				  (height-equation '(cadr tamias.string:character-size))))
  hidden
  (items (make-hash-table)) ;;hash table of ui-menu-item(s)
  ids
  active-item);;Symbol

(defstruct node-label
  )

(defstruct (ui-node (:include ui-element
			      (type 'node)
			      (x 0)
			      (x-equation 0)
			      (y 0)
			      (y-equation 0)
			      (width (* 16 32))
			      (width-equation (* 16 32))
			      (height (* 16 32))
			      (height-equation (* 16 32))
			      ))
  (labels (make-hash-table))
  input-nodes
  output-nodes)

  
		    
			      

;;ui-frame holds ui-elements


(defun get-current-active-menu (ui-menu)
  ;;recursively get the current active menu-item structure
  ;;So, assuming that File is active with no other active menu-item's menu being active, then the File menu would be returned as being active, along with it's x y widht and height
  (let ((current-active-menu ui-menu) ;;what we want is the current active menu item, not the menu
	(x (ui-element-x ui-menu))
	(y (ui-element-y ui-menu))
	(width (ui-element-width ui-menu))
	(height (ui-element-height ui-menu)))
    (loop for id in (ui-menu-ids ui-menu)
       do (let (ui-mi-menu (ui-menu-item-menu (gethash id (ui-menu-items ui-menu))))
	    ;;ui-mi-menu = ui-menu-item-menu
	    (if ui-mi-menu
		(if (ui-menu-active? ui-mi-menu)
		    (progn (setf current-active-menu (gethash id (ui-menu-items ui-menu)))
			   (incf x (ui-element-width ui-menu))
			   (incf y (* (position id (ui-menu-ids ui-menu)) (cadr tamias.string:character-size)))
			   (setf width (ui-element-width ui-mi-menu)
				 height (ui-element-height ui-mi-menu))
			   (let ((new-active nil)
				 (n-x 0)
				 (n-y 0)
				 (n-w 0)
				 (n-h 0))
			     (setf (values new-active n-x n-y n-w n-h) (get-current-active-menu ui-mi-menu))
			     (if new-active
				 (progn (setf current-active-menu new-active)
					(incf x n-x)
					(incf y n-y)
					(setf width n-w
					      height n-h))))
			   (values current-active-menu x y width height))))))
    ))


;;Note on future docker implementation:
;;  X-equation and such are just the x and y location.


;;If you are adding a ui-element to a ui-frame, just use numbers in relation to the offset of the ui-frame
;;So, if you want to draw a ui-button inside a ui-frame and the x/y of ui-button is 0/0 in relation to ui-frame, then just use 0/0


#|  `(progn (defvar ,name (make-ui-element :x (+ ,x 6) :x-equation `,',x
:y (+ ,y 6) :y-equation `,',y
:width (- ,w 12) :width-equation `,',w
:height (- ,h 12) :height-equation `,',h
:action ,action :special ,special :text ,text))
(push (quote ,name) (get-hash ,elements-list elements))))|#
(defgeneric action.hover (ui-element ui-type)
  (:method (ui-element ui-type)
    nil)
  ;;draw the element again but lighter? Uhhh, something like that I guess?
  ;;Actually, yeah. Unless it's a light color, then use a darker color (in fact, +/- 40 dependent on </> 127)  
  )

(defgeneric action.click (ui-element ui-type)
  (:method (ui-element ui-type)
    (print tamias:*mouse-x*))
  ;;chekc mouse-x mouse-y, if action, funcall action, with ui-element as first arg
  )

(defgeneric render.ui (ui-element ui-type &optional hover)
  (:method (ui-element ui-type &optional hover)
    nil)
  )


#|(defun render-ui-elements ()
(render-ui ui-menu-bar 'menu-bar)
(loop for ui-element in (gethash current-element elements)
   do (render-ui ui-element (ui-element-type ui-element))))
|#
