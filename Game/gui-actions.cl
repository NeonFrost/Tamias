#|

The flow

Note on mouse: create a boundary box for the mouse, i.e. from -3 to +3 of the mouse-x and mouse-y

Click in window -> tamias.gui.mouse-click -> check if menu-bar is active item? -> No? If item is located under click area, 

|#

;;action.click assumes that the mouse-click is within the confines of the ui-element

(defvar clicked-area '(0 0))

(defmethod action.click (ui-element (ui-type (eql 'menu-bar)))
  (let ((x-acc 0)
	(x-point tamias:*mouse-x*))
    (loop for item in (ui-menu-bar-ids ui-element)
       do (let ((str-size (* (car tamias.string:character-size) (length item))))
	    (if (and (>= x-point x-acc)
		     (<= x-point (+ x-acc str-size)))
		(return (setf (ui-menu-bar-active-item ui-element) item))
		(incf x-acc str-size))))))
  ;;Check mouse-x against each item, set it to the active item and then drop down/open right, unless the active item is clicked, then deactivate the active-item, aka set to 0
  ;;note: the menu-bar sets the substate to 'menu-bar. Only check against y when the sub-state is 'menu-bar
  ;;If the mouse-x and mouse-y are outside the region of the possible menu-bar-items, it deactivates the menu-bar and deactivates all the other things, and sets sub-state to 'top
(defmethod action.click (ui-element (ui-type (eql 'entry)))
  (setf current-text-context (ui-element-entry ui-element)))
;;set current text stuff to the ui-element-entry
(defmethod action.click (ui-element (ui-type (eql 'spin-box)))
  ;;check which region got clicked
  (let ((x-max (+ (ui-base-x ui-element) (ui-base-width ui-element)))
	(y-max (+ (ui-base-x ui-element) (ui-base-height ui-element))))
    (if (and (>= tamias:*mouse-x* (- x-max 16))
	     (<= tamias:*mouse-x* x-max)
	     (>= tamias:*mouse-y* (- y-max 16))
	     (<= tamias:*mouse-y* y-max))
	(if (and (>= tamias:*mouse-y* (- y-max 16))
		 (<= tamias:*mouse-y* (- y-max 8)))
	    (decf (tamias-value-value (number-entry-value (ui-element-special ui-element))))
	    (incf (tamias-value-value (number-entry-value (ui-element-special ui-element))))))))
;;(incf (tamias-value-value (ui-spin-box-value (ui-element-special ui-element))))
;;(decf (tamias-value-value (ui-spin-box-value (ui-element-special ui-element))))

;;;Forgot about this, but with the symbol, make it a spinbox with a max of 255. Use the integer to display an Ascii character

(defmethod action.click (ui-element (ui-type (eql 'menu-item)))
  (if (ui-element-action ui-element)
      (eval (ui-element-action ui-element)))
  )

(defmethod action.click (ui-element (ui-type (eql 'scroll)))
  "Check Mouse-y against the ui-element. "
  (if (< (cadr clicked-area) 16)
      "scroll up"
      (if (> (cadr clicked-area) (- (ui-element-height ui-element) 16))
	  "scroll down"
	  "Area not on scroll-bar?")))

(defmethod action.click (ui-element (ui-type (eql 'button)))
  (eval (ui-element-action ui-element)))

(defun action.prep (ui-element ui-type)
  (setf current-text-context nil
	)
  (action.click ui-element ui-type))

(defun tamias.gui.menu-bar-item.click (ui-menu-bar-item)
  (let ((inital-x (ui-menu-bar-item-x current-menu-bar-item))
	(width-accumulator (ui-menu-bar-item-width current-menu-bar-item))
	)
    
    ))

(defun gui.frame.click (element x-acc y-acc)
  (if (mouse.test-position (+ x-acc (ui-base-x element))
			   (+ y-acc (ui-base-y element))
			   (ui-base-width element)
			   (ui-base-height element)
			   'box)
   #|(and (> *mouse-x* (+ x-acc (ui-base-x element)))
	   (< *mouse-x* (+ x-acc (ui-base-x element) (ui-base-width element)))
	   (> *mouse-y* (+ y-acc (ui-base-y element)))
	   (< *mouse-y* (+ y-acc (ui-base-y element) (ui-base-height element))))|#
      (if (eq (ui-base-type element) 'frame)
	  (loop for id in (ui-frame-ids element)
	     do (let ((frame-element (gethash id (ui-frame-items element))))
		  (gui.frame.click frame-element (+ x-acc (ui-base-x element)) (+ y-acc (ui-base-y element)))))
	  (progn (setf clicked-area (list (- tamias:*mouse-x* x-acc) (- tamias:*mouse-y* y-acc)))
		 (action.click element (ui-base-type element))))))

"Go through each item in the window and see if the mouse clicked on a viable element"
(defun gui.window.click (element x-acc y-acc)
  )

"Uh huh. So, this is still in development code (great).
So gui.click.check goes through each non menu-bar element and determines what got clicked by the mouse (or pointer)
The gist is tamias.gui.click : -> menu-bar active? -> no -> is the pointer in the range of the menu-bar? -> no -> Then check which element was clicked

Ah HAH! click.check /already/ deals with activating elements. So, for example, it goes through a frames elements and if it did something, then it exits the loop
I've done a small improvement to it and added a case statement. 
If you're reading this, just remember: Documentation is important. For every second it takes to write a procedure, it requires 1 minute to decipher.
"
(defun gui.click.check ()
  (let ((ui-manager (get-ui-manager (state-symbol tamias:state) (state-sub-state tamias:state))))
;;    (print "click")
    (loop for element in (ui-manager-collection ui-manager)
       do (case (ui-base-type element)
	    (frame (if (gui.frame.click element 0 0)
		       (return t)))
	    (window (if (gui.window.click element 0 0)
			(return t)))
	    (otherwise (if (mouse.test-position (ui-base-x element) (ui-base-y element) (ui-base-width element) (ui-base-height element) 'box)
			   (return (action.prep element (ui-base-type element)))))))))


(defun tamias.gui.click ()
  "Expand this function. Check if there is a menu-bar. current-menu-bar-item makes an assumption that is too dangerous for finalized code"
  (let ((ui-menu-bar (get-menu-bar (state-symbol tamias:state) (state-sub-state tamias:state))))
    (if ui-menu-bar
	(let ((current-menu-bar-item (ui-menu-bar-active-item ui-menu-bar)))
  ;;check if the menu-bar of the gui-manager of the current state and sub-state has an active item, not '0.
	  (if current-menu-bar-item
      ;;then handle the mouse click, going through each entry in the ui-menu-bar's sub-menus and such. If the mouse-x lies outside the beginning or end of the start and finish of the sub-menus, then deactivate the menus. This is alpha code, so don't worry about making it perfect.
	      (let ((current-item (gethash current-menu-bar-item (ui-menu-bar-items ui-menu-bar))))
		(if (< tamias:*mouse-x* (ui-menu-bar-item-x current-item))
		    (setf (ui-menu-bar-active-item ui-menu-bar) nil)
		    (if (> tamias:*mouse-y* (cadr tamias.string:character-size))
			(gui.get-menu-bar-item.click current-item))))
	    ;;if no menu-bar items are active, then go through ui-manager-collection and check it against each element until it gets a hit. This is why Frames are a good idea. If a mouse-click lies outside the frame, then it goes to the next frame, and so on.
	      (if (> tamias:*mouse-y* (cadr tamias.string:character-size))
		  (gui.click.check)
		  ;;then go through each frame and check if the mouse click is on any of the underlying elements
		  (action.click ui-menu-bar 'menu-bar)
		  )
	      ))
	(gui.click.check))))

(defun ui.find-current-hover ()
  (let ((ui-message nil))
    (loop for ui-element in (ui-manager-collection (gethash (state-sub-state tamias:state) (gethash (state-symbol tamias:state) ui-managers)))
       do (if (mouse.test-position (ui-base-x ui-element) (ui-base-y ui-element) (ui-base-width ui-element) (ui-base-height ui-element) 'box)
	      (return (setf ui-message ui-element))))
    (setf (ui-manager-current-hover (get-current-ui-manager)) ui-message)))
	      

(defun tamias.gui.mouse-move ()
  #|
Change current hover (used to highlight buttons and other elements), check if menu-bar is active, if menu-bar is active, then check if the mouse is hovering over a menu-bar-item
If the mouse is hovering over a menu-bar-item, then set the current active menu-bar-item to the one it is hovering over (will change what is dropped down from the bar)
If menu-bar is active and the mouse is hovering over a sub-menu entry, highlight that entry, if not, set current-hover to nil. Do this recursively so long as a menu-item is active
  |#
  (let* ((menu-bar (get-current-menu-bar))
	 (active-menu nil))
    (if menu-bar
	(let ((menu-bar-item (ui-menu-bar-active-item menu-bar)))
	  (if menu-bar-item (setf active-menu (ui-menu-item-menu menu-bar-item)))
	  (if active-menu
	      ;;active-item
	      (let ((x 0) (y 0)
		    (w 0) (h 0))
		(setf (values active-menu x y w h) (get-current-active-menu active-menu))
		(incf x (ui-element-x menu-bar-item))
		(incf y (ui-element-y menu-bar-item))
		(setf w (ui-element-width menu-bar-item)
		      h (ui-element-height menu-bar-item))
		(if (mouse.test-position x y w h 'box)
		    ;;Set the menu-item to be highlighted
		    (setf (ui-manager-current-hover (get-current-ui-manager)) menu-bar-item)
		    (setf (ui-manager-current-hover (get-current-ui-manager)) nil)))
	      ;;no active-item
	      (if (mouse.test-position (ui-menu-bar-x menu-bar) (ui-menu-bar-y menu-bar) (ui-menu-bar-width menu-bar) (ui-menu-bar-height menu-bar))
		  ;;highlight  the current menu-bar the mouse is over
		  (loop for menu-bar-item in (ui-menu-bar-ids menu-bar)
		     do (let ((mbar-item (gethash menu-bar-item (ui-menu-bar-items menu-bar))))
			  (if (mouse.test-position (ui-element-x mbar-item) (ui-element-y mbar-item) (ui-element-width mbar-item) (ui-element-height mbar-item))
			      (return (setf (ui-manager-current-hover (get-current-ui-manager)) mbar-item)))))
		  (ui.find-current-hover))));;go through each item in the ui-manager and check the mouse against them
	(ui.find-current-hover);;go through each item in the ui-manager and check the mouse against them
		  )))
