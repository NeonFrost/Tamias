;;Eventually, I'll add in limiters
;;
;;
(defstruct id-system
  (current-id 0)
  (content (make-hash-table)))

(defmacro define-id-system (id-system)
  `(defstruct (,id-system (:include id-system))))

(defmacro add-id (content id-system)
  `(progn (setf (gethash (id-sytem-current-id ,id-system) (id-system-content ,id-system)) ,content)
	  (incf (id-system-current-id ,id-system))))
(defmacro get-system-length (id-system)
  `(id-system-current-id ,id-system))
	 
	  
