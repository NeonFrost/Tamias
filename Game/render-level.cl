(defun render-entity (entity)
  (render-box (entity-x entity)
	      (entity-y entity)
	      (entity-width entity)
	      (entity-height entity)
	      :color (entity-symbol-color entity)))

(defun render-level ()
  (loop for entity in entities
     do (render-entity (eval entity)))
  (loop for surface in surfaces
     do (render-entity (eval surface)))
  )

(add-to-state-render render-level level)
