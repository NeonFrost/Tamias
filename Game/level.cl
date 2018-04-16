(defvar started nil)

(defun process-level ()
  (loop for entity in entities
     do (if +pulling-player+
	    (move-entity (eval entity) :friction nil)
	    (move-entity (eval entity)))))
(add-loop-function process-level level 'top)

(defun render-entity (entity)
  (render-box (entity-x entity)
	      (entity-y entity)
	      (entity-width entity)
	      (entity-height entity)
	      :color (entity-symbol-color entity)))

(defun render-level ()
  (loop for surface in surfaces
     do (render-entity (eval surface)))
  (loop for entity in entities
     do (render-entity (eval entity))))
(add-to-state-render render-level level)
