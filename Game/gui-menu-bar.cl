(defun menu-bar.init (state sub-state)
  (if (not (gethash state ui-managers))
      (progn (setf (gethash state ui-managers) (make-hash-table))
	     (setf (gethash sub-state (gethash state ui-managers)) (make-ui-manager))))
  (setf (get-menu-bar state sub-state) (make-ui-menu-bar))
  )

(defun add-menu-bar-item (state sub-state item &key label action color)
  "Add's a 'top level' menu bar item, like 'File' or 'Edit'. Helper function"
  (setf (get-menu-bar-item state sub-state item)
	(make-ui-menu-item :width (* (length label) (car tamias.string:character-size)) :label label :action action :color color))
  (push item (ui-menu-bar-ids (get-menu-bar state sub-state)))
  )

(defmacro menu-bar.add-item (state sub-state item (&key (label "") action color))
  "Primary macro to add an item to the menu-bar of state and sub-state.
Example: (menu-bar.add-item 'blender 'sculpt 'tool (:label \"tool\" :color '(127 127 127 255))"
  `(add-menu-bar-item ,state ,sub-state ,item :label ,label :action ,action :color ,color)
  )

(defun add-menu-bar-sub-item (item item-parent &key (label "") action color)
  "Helper function, add's an item to a menu-bar item (or an item of a menu-bar item). Ex: blender -> menu-bar -> File -> import -> fbx"
  (setf (get-menu-bar-item-sub-item item item-parent)
	(make-ui-menu-item :width (* (length label) (car tamias.string:character-size)) :label label :action action :color color))
  (push item (ui-menu-item-ids item-parent)))

(defmacro menu-bar.item.add-item (item item-parent (&key (label "") action color))
  "add's an item to a menu-bar item (or an item of a menu-bar item). 
Example: (menu-bar.item.add-item 'import (get-menu-bar-item 'blender 'sculpt 'file) (:label \"import\"))"
  `(add-menu-bar-sub-item ,item ,item-parent :label ,label :action ,action :color ,color))

