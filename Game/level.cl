(defvar started nil)

(defun process-level ()
  (loop for surface in surfaces
       do `(move-entity ,surface))
  (loop for entity in entities
     do `(move-entity ,entity))
  )
  #|       (loop for entity-b in (remove entity entities)
  do (if (test-collision entity entity-b)
  (progn (decf (entity-hp entity) 10)
  (decf (entity-hp entity-b) 10))))
  |#
;;  (process-object ball))
  
