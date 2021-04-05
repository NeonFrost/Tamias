(defvar ui-managers (make-hash-table))
(defvar current-ui-state (state-symbol tamias:state))
(defvar current-ui-id 0)

(defmacro get-ui-manager (state sub-state)
  `(gethash ,sub-state (gethash ,state ui-managers)))

(defmacro get-current-menu-bar ()
  (ui-manager-menu-bar (get-ui-manager (state-symbol tamias:state) (state-sub-state tamias:state))))

(defmacro get-menu-bar (state sub-state)
  `(ui-manager-menu-bar (get-ui-manager ,state ,sub-state)))

(defmacro get-menu-bar-item (state sub-state item)
  `(gethash ,item (ui-menu-bar-items (get-menu-bar ,state ,sub-state))))

(defmacro get-menu-bar-items (state sub-state)
  `(ui-menu-bar-items (get-menu-bar ,state ,sub-state)))

(defmacro get-menu-bar-sub-items (item)
  `(ui-menu-item-items ,item))

(defmacro get-menu-bar-item-sub-item (item parent-item)
  `(gethash ,item (get-menu-bar-sub-items ,parent-item)))

(defmacro get-current-ui-manager ()
  `(gethash (state-sub-state tamias:state) (gethash (state-symbol tamias:state) ui-managers)))

