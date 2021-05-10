(defun render.ui-elements (state &optional sub-state)
  (if (not sub-state)
      (if (not (symbolp state))
	  (setf sub-state (state-sub-state state)
		state (state-symbol state))
	  (setf sub-state (state-sub-state tamias:state)))
      (if (not (symbolp state))
	  (setf state (state-symbol state))))
  (loop for ui-element in (ui-manager-collection (gethash sub-state (gethash state ui-managers)))
     do (render.ui ui-element (ui-element-type ui-element)))
  (if (ui-manager-current-hover (get-current-ui-manager))
      (render.ui (ui-manager-current-hover (get-current-ui-manager)) (ui-base-type (ui-manager-current-hover (get-current-ui-manager))) t))
  )

(defun state.init-ui (state sub-state &rest ui-elements)
  "Use symbols for state and sub-state. ui-elements are not necessary"
  (if (not (gethash state ui-managers))
      (setf (gethash state ui-managers) (make-hash-table)))
  (setf (gethash sub-state (gethash state ui-managers)) (make-ui-manager))
  (if ui-elements
      (setf (ui-manager-collection (gethash sub-state (gethash state ui-managers))) ui-elements))
  )

(defmacro ui-element.make (state sub-state (type x y w h &key action special label entry))
  `(push (make-ui-element :type ,type :ID current-ui-id
			  :x ,x :x-equation `,',x
			  :y ,y :y-equation `,',y
			  :width ,w :width-equation `,',w
			  :height ,h :height-equation `,',h
			  :action ,action :special ,special
			  :label ,label :entry ,entry)
	 (ui-manager-collection (get-ui-manager ,state ,sub-state)))
  (incf current-ui-id))

(defmacro ui-spin-box.make (state sub-state (x y w h &key action))
  `(ui-element.make ,state ,sub-state ('spin-box ,x ,y ,w ,h :action ,action :special (make-number-entry) :label "0")))

(defmacro ui-frame.make (state sub-state (x y w h items))
  `(push (make-ui-frame :id current-ui-id
			:x ,x :x-equation `,',x
			:y ,y :y-equation `,',y
			:width ,w :width-equation `,',w
			:height ,h :height-equation `,',h
			:items ,items)
	 (ui-manager-collection (get-ui-manager ,state ,sub-state)))
  (incf current-ui-id))

#|

Example:
(get-menu-bar-sub-items (get-menu-bar-item '3d-editor 'sculpt 'file))
(get-menu-bar-sub-item 'new (get-menu-bar-item '3d-editor 'sculpt 'file))

|#

(defun add-state-ui-element (state sub-state ui-element)
  (push ui-element (ui-manager-collection (gethash sub-state (gethash state ui-managers)))))

(defmacro ui-manager.add-element (state sub-state ui-element)
  `(push ,ui-element (ui-manager-collection (gethash ,sub-state (gethash ,state ui-managers)))))

(defmacro ui-manager.define-element (state sub-state (type x y w h &key action special label entry))
  `(add-state-ui-element ,state ,sub-state (make-ui-element :type ,type :ID current-ui-id
							  :x ,x :x-equation `,',x
							  :y ,y :y-equation `,',y
							  :width ,w :width-equation `,',w
							  :height ,h :height-equation `,',h
							  :action ,action :special ,special
							  :label ,label :entry ,entry)))

(defun ui-manager.add-frame (state sub-state x y w h &rest items)
  (ui-manager.add-element state sub-state (ui-frame.make state sub-state (x y w h items))))

(defun add-state-ui-elements (state sub-state &rest ui-elements)
  (loop for ui-element in ui-elements
       do (add-state-ui-element state sub-state ui-element)))

(defun menu.set-width (menu)
  (let ((highest 0))
    (loop for id in (ui-menu-ids menu)
       do (setf id (write-to-string id))
	 (if (> (length id) highest)
	      (setf highest (length id))))
    (setf (ui-menu-width menu) (* (car tamias.string:character-size) highest))))

(defun menu.recurssive-width (menu)
  (menu.set-width menu)
  (loop for id in (ui-menu-ids menu)
     do (let* ((menu-item (gethash id (ui-menu-items menu)))
	       (menu-item-menu (ui-menu-item-menu menu-item)))
	  (if menu-item-menu
	      (menu.recurssive-width menu-item-menu)))))

(defun update-window-size ()
  (setf tamias:screen-width (car (nth tamias:resolution tamias:resolution-list))
	tamias:screen-height (cadr (nth tamias:resolution tamias:resolution-list)))
  (sdl2:set-window-size tamias:default-window tamias:screen-width tamias:screen-height)
  ;;  (gl:viewport 0 0 tamias:screen-width tamias:screen-height)
  (let ((state-ui-manager (gethash (state-symbol tamias:state) ui-managers)))
    (if state-ui-manager
	(if (gethash (state-sub-state tamias:state) state-ui-manager)
	    (let ((gui-manager (get-ui-manager (state-symbol tamias:state)
					       (state-sub-state tamias:state))))
	      (if gui-manager
		  (loop for element in (ui-manager-collection gui-manager)
		     do (setf (ui-element-x element) (floor (eval (ui-element-x-equation
								   element)))
			      (ui-element-y element) (floor (eval (ui-element-y-equation
								   element)))
			      (ui-element-width element) (floor (eval (ui-element-width-equation
								       element)))
			      (ui-element-height element) (floor (eval (ui-element-height-equation
									element))))
		       (if (eq (type-of element) 'ui-frame)
			   (loop for frame-element in (ui-frame-items element)
			      do  (setf (ui-element-x frame-element) (floor (eval (ui-element-x-equation
										  frame-element)))
				       (ui-element-y frame-element) (floor (eval (ui-element-y-equation
										  frame-element)))
				       (ui-element-width frame-element) (floor (eval (ui-element-width-equation
										      frame-element)))
				       (ui-element-height frame-element) (floor (eval (ui-element-height-equation
										       frame-element))))))))
	      (let ((mbar (ui-manager-menu-bar gui-manager)))
		(if mbar
		    (progn (setf (ui-menu-bar-width mbar)
				 (eval (ui-menu-bar-width-equation mbar)))
			   (loop for menu-item in (ui-menu-bar-ids mbar)
			      do (let ((menu (ui-menu-item-menu (gethash (ui-menu-bar-items menu-item)))))
				   (menu.recurssive-width menu)))))))))))
    
    #|  (loop for menu in **tamias-menus**
    do (setf (menu-x menu) (+ (eval (menu-x-equation menu)) 6)
    (menu-y menu) (+ (eval (menu-y-equation menu)) 6)
    (menu-width menu) (- (eval (menu-width-equation menu)) 12)
    (menu-height menu) (- (eval (menu-height-equation menu)) 12))))
    |#

