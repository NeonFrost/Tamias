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


(defvar plane-2 (let* ((vertices (list (make-vertex :values (make-array '(3) :initial-contents '(-30.0 10.0 10.0)))
				     (make-vertex :values (make-array '(3) :initial-contents '(30.0 -10.0 10.0)))
				     (make-vertex :values (make-array '(3) :initial-contents '(-30.0 -20.0 -60.0)))
				     (make-vertex :values (make-array '(3) :initial-contents '(30.0 20.0 -60.0)))))
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

(setf (model-normal-vertices plane-2) (model-vertices plane-2)
      (model-texture-vertices plane-2) (model-vertices plane-2))

(loop for polygon below 2
	       do (loop for point below 3
			do (setf (point-normal-index (elt (polygon-points (elt (object-polygons (elt (model-objects plane-2) 0)) polygon)) point))
				 (point-position-index (elt (polygon-points (elt (object-polygons (elt (model-objects plane-2) 0)) polygon)) point))
				 (point-texture-index (elt (polygon-points (elt (object-polygons (elt (model-objects plane-2) 0)) polygon)) point))
				 (point-position-index (elt (polygon-points (elt (object-polygons (elt (model-objects plane-2) 0)) polygon)) point)))))
