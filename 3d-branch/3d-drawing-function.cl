#|
Model-group needs to be implemented
Shaders will be bound per model group
|#
(defun draw-vao (model)
#| 
 (let ((rot-x (+ world-rotation-x world-rotation-inc-x))
	(rot-y (+ world-rotation-y world-rotation-inc-y))
	(rot-z (+ world-rotation-z world-rotation-inc-z)))
|#
    (loop for object in (model-objects model)
       do
	 #|(if (or (< (object-rotation-x object) -360)
		  (> (object-rotation-x object) 360))
	      (setf (object-rotation-x object) 0))
	 (if (or (< (object-rotation-y object) -360)
		 (> (object-rotation-y object) 360))
	     (setf (object-rotation-y object) 0))
	 (if (or (< (object-rotation-z object) -360)
		 (> (object-rotation-z object) 360))
	     (setf (object-rotation-z object) 0))
	 (gl:translate (+ (object-x object) (model-x model)) (+ (model-y model) (object-y object)) (+ (model-z model) (object-z object)))
	 (gl:rotate (+ (object-rotation-x object) (model-rotation-x model)) 1.0 0.0 0.0)
	 (gl:rotate (+ (object-rotation-y object) (model-rotation-y model)) 0.0 1.0 0.0)
	 (gl:rotate (+ (object-rotation-z object) (model-rotation-z model)) 0.0 0.0 1.0)
	 (gl:scale (* (object-width object) (model-scale-scalar model))
		   (* (object-height object) (model-scale-scalar model))
		   (* (object-depth object) (model-scale-scalar model)))
|#
;;       (gl:bind-vertex-array (object-vao object))
	 (gl:bind-buffer :array-buffer (car (object-vbo-ids object)))
       ;;     (gl:vertex-attrib-pointer 0 3 :float nil 3 (cffi:null-pointer))
;;         (gl:enable-vertex-attrib-array 0)
       ;;	 (gl:draw-arrays :triangles 0 4)
	 (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)
#|	 (gl:translate (- (+ (object-x object) (model-x model))) (- (+ (object-y object)  (model-y model))) (- (+ (object-z object) (model-z model))))
	 (gl:load-identity)
	 (gl:translate (+ 0.0 camera-x) (+ -50.0 camera-y) (+ -300 camera-z))
	 (gl:rotate rot-x 1.0 0.0 0.0)
	 (gl:rotate rot-y 0.0 1.0 0.0)
	 (gl:rotate rot-z 0.0 0.0 1.0)
	 )
|#))

(defun draw-vaos (&rest models)
  (loop for model in models
     do (draw-vao model)))

(defun alt-draw (&rest models)
  (let ((rot-x (+ world-rotation-x world-rotation-inc-x))
	(rot-y (+ world-rotation-y world-rotation-inc-y))
	(rot-z (+ world-rotation-z world-rotation-inc-z))
	(tri-count 0))
    (if (> rot-x 360)
	(setf world-rotation-x 0))
    (if (> rot-y 360)
	(setf world-rotation-y 0))
    (if (> rot-z 360)
	(setf world-rotation-z 0))
    #|    (setf world-rotation-inc-x (+ world-rotation-inc-x 5))
    (setf world-rotation-inc-y (+ world-rotation-inc-y 5))|#
    (gl:rotate rot-x 1.0 0.0 0.0)
    (gl:rotate rot-y 0.0 1.0 0.0)
    (gl:rotate rot-z 0.0 0.0 1.0)
    (loop for model in models
       do (let ((animated? (if (model-animation model)
			       t
				nil))
		(trans-bones nil)
		(alt-verts nil))
	    (if animated?
		(let ((num-bones (cadr (array-dimensions (tja-data-frames (model-animation model))))))
		  (setf trans-bones (make-array (list num-bones) :initial-element (make-mat4)))
		  (loop for n below num-bones
		     do (setf (aref trans-bones n) (calc-frame-mat model n trans-bones)))
		  (if (not bones-printed?)
		      (progn (setf bones-printed? t)
			     (print trans-bones)))))
	    (if animated?
		(let ((num-verts (car (array-dimensions (model-vertices model)))))
		  (setf alt-verts (make-array (list num-verts)))
		  (loop for n below num-verts
		     do (let ((tmp-vec nil))
			  (setf tmp-vec (calc-frame-vals model n (vertex-values (aref (model-vertices model) n)) trans-bones))
			  (setf (aref alt-verts n) (make-vertex :values (make-array '(3) :initial-contents (list (aref tmp-vec 0) (aref tmp-vec 1) (aref tmp-vec 2)))))))))
	    (loop for object in (model-objects model)
	       do (let ((texture-id (object-texture-id object)))
		    (gl:material :front :ambient-and-diffuse (list (object-r object) (object-g object) (object-b object) 1))
		    (gl:material :front :specular '(1.0 1.0 1.0 .6))
		    (gl:material :front :shininess 96)
		    #|	 
		    (if (eq (length (object-vertices object)) 4)
		    (gl:material :front :emission '(0.5 .0 .0 0.1))
		    (gl:material :front :emission '(0.0 .0 .0 0.0)))
		    |#
		    (if (or (< (object-rotation-x object) -360)
			    (> (object-rotation-x object) 360))
			(setf (object-rotation-x object) 0))
		    (if (or (< (object-rotation-y object) -360)
			    (> (object-rotation-y object) 360))
			(setf (object-rotation-y object) 0))
		    (if (or (< (object-rotation-z object) -360)
			    (> (object-rotation-z object) 360))
			(setf (object-rotation-z object) 0))
		    ;;	       (gl:translate (model-x model) (model-y model) (model-z model))
		    (gl:translate (+ (object-x object) (model-x model)) (+ (model-y model) (object-y object)) (+ (model-z model) (object-z object)))
		    (gl:rotate (+ (object-rotation-x object) (model-rotation-x model)) 1.0 0.0 0.0)
		    (gl:rotate (+ (object-rotation-y object) (model-rotation-y model)) 0.0 1.0 0.0)
		    (gl:rotate (+ (object-rotation-z object) (model-rotation-z model)) 0.0 0.0 1.0)
		    ;;	       (gl:translate (- (model-x model)) (- (model-y model)) (- (model-z model)))
		    ;;	       (gl:translate (- (+ (object-x object) (model-x model))) (- (+ (object-y object)  (model-y model))) (- (+ (object-z object) (model-z model))))
		    (gl:scale (* (object-width object) (model-scale-scalar model))
			      (* (object-height object) (model-scale-scalar model))
			      (* (object-depth object) (model-scale-scalar model)))
		    (if texture-id
			(progn (gl:bind-texture :texture-2d texture-id)
			       (gl:push-matrix)
			       (gl:matrix-mode :texture)
			       (gl:load-identity)
			       (gl:scale 1.0 -1.0 1.0)
			       (gl:matrix-mode :modelview)
			       (gl:pop-matrix)
			       ;;(gl:blend-func :src-alpha :one)
			       ))
		    ;;(gl:blend-func :src-alpha :zero))
		    (gl:begin :triangles)
		    (if (object-polygons object)
			(loop for n below (car (array-dimensions (object-polygons object)))
			   ;;tri in (object-polygons object)
			   do (let* ((tri (aref (object-polygons object) n))
				     (va (polygon-vertex-a model tri))
				     (vb (polygon-vertex-b model tri))
				     (vc (polygon-vertex-c model tri))
				     (normal? (point-normal-index (polygon-a tri))))
				(incf tri-count)
				(if animated?
				    (let ((vna (aref alt-verts (position-a tri)))
					  (vnb (aref alt-verts (position-b tri)))
					  (vnc (aref alt-verts (position-c tri))))
				      #|				  (let ((vna (make-vertex))
				      (vnb (make-vertex))
				      (vnc (make-vertex))
				      (vals (make-array '(3)))
				      (ret-vals (vertex-values va)))
				      (setf vals (calc-frame-vals model (position-a tri) ret-vals trans-bones))
				      (loop for i below 3
				      do (setf (aref (vertex-values vna) i) (aref vals i)))
				      (setf vals (make-array '(3))
				      ret-vals (vertex-values vb))
				      (setf vals (calc-frame-vals model (position-b tri) ret-vals trans-bones))
				      (loop for i below 3
				      do (setf (aref (vertex-values vnb) i) (aref vals i)))
				      (setf vals (make-array '(3))
				      ret-vals (vertex-values vc))
				      (set(mat4*mat4 t-mat trans-mat)f vals (calc-frame-vals model (position-c tri) ret-vals trans-bones))
				      (loop for i below 3
				      do (setf (aref (vertex-values vnc) i) (aref vals i)))|#
				      (if texture-id
					  (render-normal-texture model tri vna vnb vnc)
					  (progn (gl:normal (normal-x (polygon-normal-a model tri))
							    (normal-y (polygon-normal-a model tri))
							    (normal-z (polygon-normal-a model tri)))
						 (gl:vertex (vertex-x vna) (vertex-y vna) (vertex-z vna))
						 (gl:normal (normal-x (polygon-normal-b model tri))
							    (normal-y (polygon-normal-b model tri))
							    (normal-z (polygon-normal-b model tri)))
						 (gl:vertex (vertex-x vnb) (vertex-y vnb) (vertex-z vnb))
						 (gl:normal (normal-x (polygon-normal-c model tri))
							    (normal-y (polygon-normal-c model tri))
							    (normal-z (polygon-normal-c model tri)))
						 (gl:vertex (vertex-x vnc) (vertex-y vnc) (vertex-z vnc))))
				      )
				    (if texture-id
					(if normal? ;;(point-normal-vertex (polygon-a tri))
					    (render-normal-texture model tri va vb vc)
					    (let ((normal-vec (calculate-normal-vecs va vb vc)))
					      ;;(gl:normal (aref normal-vec 0) (aref normal-vec 1) (aref normal-vec 2))
					      (gl:tex-coord (vertex-u (polygon-texture-a model tri))
							    (vertex-v (polygon-texture-a model tri)))
					      (gl:vertex (+ (vertex-x va) (object-x object)) (+ (vertex-y va) (object-y object)) (+ (vertex-z va) (object-z object)))
					      (gl:tex-coord (vertex-u (polygon-texture-b model tri))
							    (vertex-v (polygon-texture-b model tri)))
					      (gl:vertex (+ (vertex-x vb) (object-x object)) (+ (vertex-y vb) (object-y object)) (+ (vertex-z vb) (object-z object)))
					      (gl:tex-coord (vertex-u (polygon-texture-c model tri))
							    (vertex-v (polygon-texture-c model tri)))
					      (gl:vertex (+ (vertex-x vc) (object-x object)) (+ (vertex-y vc) (object-y object)) (+ (vertex-z vc) (object-z object)))))
					(if normal? ;;(point-normal-vertex (polygon-a tri))
					    (progn (gl:normal (normal-x (polygon-normal-a model tri))
							      (normal-y (polygon-normal-a model tri))
							      (normal-z (polygon-normal-a model tri)))
						   (gl:vertex (+ (vertex-x va) (object-x object)) (+ (vertex-y va) (object-y object)) (+ (vertex-z va) (object-z object)))
						   (gl:normal (normal-x (polygon-normal-b model tri))
							      (normal-y (polygon-normal-b model tri))
							      (normal-z (polygon-normal-b model tri)))
						   (gl:vertex (+ (vertex-x vb) (object-x object)) (+ (vertex-y vb) (object-y object)) (+ (vertex-z vb) (object-z object)))
						   (gl:normal (normal-x (polygon-normal-c model tri))
							      (normal-y (polygon-normal-c model tri))
							      (normal-z (polygon-normal-c model tri)))
						   (gl:vertex (+ (vertex-x vc) (object-x object)) (+ (vertex-y vc) (object-y object)) (+ (vertex-z vc) (object-z object))))
					    (let ((normal-vec (calculate-normal-vecs va vb vc)))
					      ;;(gl:normal (aref normal-vec 0) (aref normal-vec 1) (aref normal-vec 2))
					      (gl:vertex (+ (vertex-x va) (object-x object)) (+ (vertex-y va) (object-y object)) (+ (vertex-z va) (object-z object)))
					      (gl:vertex (+ (vertex-x vb) (object-x object)) (+ (vertex-y vb) (object-y object)) (+ (vertex-z vb) (object-z object)))
					      (gl:vertex (+ (vertex-x vc) (object-x object)) (+ (vertex-y vc) (object-y object)) (+ (vertex-z vc) (object-z object))))))))))
		    (gl:end)
		    (gl:bind-texture :texture-2d 0)
		    (gl:translate (- (+ (object-x object) (model-x model))) (- (+ (object-y object)  (model-y model))) (- (+ (object-z object) (model-z model))))
		    (gl:load-identity)
		    (gl:translate (+ 0.0 camera-x) (+ -50.0 camera-y) (+ -300 camera-z))
		    (gl:rotate rot-x 1.0 0.0 0.0)
		    (gl:rotate rot-y 0.0 1.0 0.0)
		    (gl:rotate rot-z 0.0 0.0 1.0)
		    )
	       ;;"Then here, we unbind the VAO/VBO"
		 )))
	 (setf polygon-count tri-count)))
