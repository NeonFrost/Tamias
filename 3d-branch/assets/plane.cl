(defvar plane (let* ((vertices (list (make-vertex :values (make-array '(3) :initial-contents '(30 0 -30)))
				     (make-vertex :values (make-array '(3) :initial-contents '(-30 0 -30)))
				     (make-vertex :values (make-array '(3) :initial-contents '(30 0 30)))
				     (make-vertex :values (make-array '(3) :initial-contents '(-30 0 30)))))
		     (polygons (list (make-polygon) ;;:a (nth 0 vertices) :b (nth 2 vertices) :c (nth 3 vertices))
				     (make-polygon))));; :a (nth 0 vertices) :b (nth 1 vertices) :c (nth 3 vertices)))))
		(setf (point-position-index (polygon-a (nth 0 polygons))) 3
		      (point-position-index (polygon-b (nth 0 polygons))) 2
		      (point-position-index (polygon-c (nth 0 polygons))) 1
		      (point-position-index (polygon-a (nth 1 polygons))) 2
		      (point-position-index (polygon-b (nth 1 polygons))) 1
		      (point-position-index (polygon-c (nth 1 polygons))) 0)
		(make-model :vertices (make-array (list (length vertices)) :initial-contents vertices)
			    :objects (list (make-object :name "Plane" :polygons (make-array (list (length polygons)) :initial-contents polygons)
							:color (make-array '(3) :initial-contents '(0.5 0.5 0.5)))))))
(setf (model-y plane) -60)
