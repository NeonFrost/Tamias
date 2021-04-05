(defun to-gl-array (array type &key (num-values 1))
  (let* ((arr-len (car (array-dimensions array)))
	 (ret-array (gl:alloc-gl-array type (* arr-len num-values))))
    (dotimes (i arr-len)
      (dotimes (j num-values)
	(setf (gl:glaref ret-array (+ i j)) (aref array j))))
    ret-array))

;;
;;NOTE: I'll be writing multiple functions that take in a keyword (:animated-bones :static)
;;
"Alright, so, the bone animations will be a pain in the butt. In fact, there's going to be a bit of wasted data, unfortunately.
The plan is to have a maximum of 4 bone influences per vertex. The bone_indice will 'preceed' the weights. 
Hypothetically, you'll be passing along the inverted bone poses and the current bone poses to the GPU. 
As of right now, I'm not sure how you'd get the transformed mat onto the GPU in a timely fashion.
The planned data layout is as follows:
Position . Normal . UV . bone_indice . Weights

Also, I'll be doing a couple different versions of the create-vao function, at the very least, one that deals with static and one that deals with animated (by bones)
Same for shader
Then, the engine flow will be
tier A models (i.e. static/bone)
tier B models
tier C models
...
which are then subdivided into sub-categories, i.e. do they use different shaders? The ones that use the same shaders should be grouped together, so that the models that require the feather shader are grouped together so that the feather shader stays 'in memory'
"

(defun create-vao (model)
  (let* ((buffers (gl:gen-buffers 4))
	 (position-buffer (elt buffers 0))
	 (normal-buffer (elt buffers 1))
	 (texture-buffer (elt buffers 2))
	 (index-buffer (elt buffers 3))
	 (vao (gl:gen-vertex-array))
	 (index-length 0))
    (loop for object in (model-objects model)
       do (incf index-length (car (array-dimensions (object-polygons object)))))
    (setf index-length (* index-length 3))
;;      (setf index-array (gl:alloc-gl-array :unsigned-short index-length))
    ;;      (gl:bind-buffer :element-array-buffer index-buffer)
    (let ((position-array nil)
	  (normal-array nil)
	  (texture-array nil)
	  (cur-ind 0)
	  (2d-offset 0)
	  (3d-offset 0))
      (setf index-array (gl:alloc-gl-array :unsigned-short index-length 3)
	    position-array (gl:alloc-gl-array :float (* index-length 3))
	    normal-array (gl:alloc-gl-array :float (* index-length 3))
	    uv-array (gl:alloc-gl-array :float (* index-length 2)))
      (loop for object in (model-objects model)
	 do (loop for polygon in (object-polygons object)
	       do (loop for point in (polygon-points polygon)
		     do (setf (gl:glaref position-array 3d-offset) (aref (aref (model-vertices model) (point-position-index point)) 0)
			      (gl:glaref normal-array 3d-offset) (aref (aref (model-normal-vertices model) (point-normal-index point)) 0)
			      (gl:glaref uv-array 2d-offset) (aref (aref (model-texture-vertices model) (point-texture-index point)) 0)
			      (gl:glaref position-array (1+ 3d-offset)) (aref (aref (model-vertices model) (point-position-index point)) 1)
			      (gl:glaref normal-array (1+ 3d-offset)) (aref (aref (model-normal-vertices model) (point-normal-index point)) 1)
			      (gl:glaref uv-array (1+ 2d-offset)) (aref (aref (model-texture-vertices model) (point-texture-index point)) 1)
			      (gl:glaref position-array (+ 3d-offset 2)) (aref (aref (model-vertices model) (point-position-index point)) 2)
			      (gl:glaref normal-array (+ 3d-offset 2)) (aref (aref (model-normal-vertices model) (point-normal-index point)) 2)
			      (gl:glaref index-array cur-ind) cur-ind)
		       (incf 2d-offset 2)
		       (incf 3d-offset 3)
		       (incf cur-ind))))
      (gl:bind-buffer :array-buffer position-buffer)
      (gl:buffer-data :array-buffer :static-draw position-array)
      (gl:bind-buffer :array-buffer normal-buffer)
      (gl:buffer-data :array-buffer :static-draw normal-array)
      (gl:bind-buffer :array-buffer texture-buffer)
      (gl:buffer-data :array-buffer :static-draw uv-array)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer index-buffer)
      (gl:buffer-data :element-array-buffer :static-draw index-array)
      (gl:free-gl-array index-array)
      (gl:free-gl-array position-array)
      (gl:free-gl-array normal-array)
      (gl:free-gl-array uv-array)
      (gl:bind-buffer :element-array-buffer 0))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer position-buffer)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-buffer :array-buffer normal-buffer)
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 3 :float nil 0 (cffi:null-pointer))
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-buffer :array-buffer texture-buffer)
    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 2 :float nil 0 (cffi:null-pointer))
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-buffer :element-array-buffer index-buffer)
    (gl:bind-vertex-array 0)
    (setf (model-vao model) vao)
    ))
#|
(defun create-vao (model)
  ;;So, what we will do here, is that we will be binding a VAO/VBO when a model is reference here. This VAO/VBO will be created on model creation
  ;;Also, this is going to be more complicated since I'm going to be using glsl 1.3 core
  ;;first ya gotta bind that shit, then ya gotta bind da index to da shader
  
  #|!!!!!!!!!!!!!!!!!!!!!!!!!!! CONDITIONAL HERE!!!!!!!!!!!!!!!!!!!!!!!!!|#
  #|!!!!!!!!!!CONDITION NEEDS TO CHECK FOR VERTICES OF MODEL, THEN OBJ NEEDS TO BE CHECKED FOR TEXTURE!!!!!!!!!!!!!|#
  #|!!THIS ALSO DPENEDS ON IF IT'S AN ANIMATED MODEL OR NOT!!|#
  #|
  Also, headache time. So, what I /can/ do is create each vbo based on the highest number out of the pos, norm, or tex coords.
  Next, after creating the three vbos of n-length, we loop through the models objects and set pos-b[i], norm-b[i]... to whatever the triangles indices values are
  i.e. we have 2000 positions, but 3000 uvs. So we create 2 vbos of length 3000. Next, we go through the triangles information.
       then, we get the vert data at that indice of each vertex array. so, 1/3/2 would get pos[1] tex[3] norm[2], making pos-b[0],norm-b[0],tex-b[0]
       thus making 'vertex' 0 be: (1.0 .5 .7) (1.0 1.0 -1.0) (.512 .32)
  In fact, I'm just going to do that.
  It'll be easier than fixing the goddamn obj-loader
  The next problem, however, is better. Adding in bones, weights and animation
  Y'know, it'll only be a massive pain in the ass, and it's going to suck up video memory, but hey, at least your processor won't have to work as hard //shrug//
  However, with that said, I'll actually create a secondary  function that does this. This function will be commente out
  |#
  (let* ((buffers (gl:gen-buffers 6))
	 (position-buffer (elt buffers 0))
	 (normal-buffer (elt buffers 1))
	 (texture-buffer (elt buffers 2))
	 (pi-buffer (elt buffers 3))
	 (ni-buffer (elt buffers 4))
	 (ti-buffer (elt buffers 5))
	 (vertex-array nil))
    (gl:bind-buffer :array-buffer position-buffer)
    (let* ((v-length (car (array-dimensions (model-vertices model))))
	   (p-arr (gl:alloc-gl-array :float (* v-length 3))))
      (loop for i below v-length
	 do (setf (gl:glaref p-arr i) (aref (aref (model-vertices model) i) 0)
		  (gl:glaref p-arr (+ i 1)) (aref (aref (model-vertices model) i) 1)
		  (gl:glaref p-arr (+ i 2)) (aref (aref (model-vertices model) i) 2)))
      (gl:buffer-data :array-buffer :static-draw p-arr)
      (gl:free-gl-array p-arr))
    (gl:bind-buffer :array-buffer 0)
    
    (gl:bind-buffer :array-buffer normal-buffer)
    (let* ((v-length (car (array-dimensions (model-normal-vertices model))))
	   (n-arr (gl:alloc-gl-array :float (* v-length 3))))
      (loop for i below v-length
	 do (setf (gl:glaref n-arr i) (aref (aref (model-normal-vertices model) i) 0)
		  (gl:glaref n-arr (+ i 1)) (aref (aref (model-normal-vertices model) i) 1)
		  (gl:glaref n-arr (+ i 2)) (aref (aref (model-normal-vertices model) i) 2)))
      (gl:buffer-data :array-buffer :static-draw n-arr)
      (gl:free-gl-array n-arr))
    (gl:bind-buffer :array-buffer 0)
    
    (gl:bind-buffer :array-buffer texture-buffer)
    (let* ((v-length (car (array-dimensions (model-texture-vertices model))))
	   (n-arr (gl:alloc-gl-array :float (* v-length 2))))
      (loop for i below v-length
	 do (setf (gl:glaref n-arr i) (aref (aref (model-texture-vertices model) i) 0)
		  (gl:glaref n-arr (+ i 1)) (aref (aref (model-texture-vertices model) i) 1)))
      (gl:buffer-data :array-buffer :static-draw n-arr)
      (gl:free-gl-array n-arr))
    (gl:bind-buffer :array-buffer 0)	 
    (let ((index-length 0)
	  (position-array nil)
	  (normal-array nil)
	  (texture-array nil)
	  (cur-ind 0))
      (loop for object in (model-objects model)
	 do (incf index-length (car (array-dimensions (object-polygons object)))))
      (setf position-array (gl:alloc-gl-array :unsigned-short (* index-length 3)))
      (setf normal-array (gl:alloc-gl-array :unsigned-short (* index-length 3)))
      (setf texture-array (gl:alloc-gl-array :unsigned-short (* index-length 3)))
;;      (gl:bind-buffer :element-array-buffer index-buffer)
      (loop for object in (model-objects model)
	 do (loop for polygon in (object-polygons object)
	       do (loop for point in (polygon-points polygon)
		     do (setf (gl:glaref position-array cur-ind) (point-position-index point)
			      (gl:glaref normal-array cur-ind) (point-normal-index point)
			      (gl:glaref texture-array cur-ind) (point-texture-index point))
		       (incf cur-ind))))
      (gl:bind-buffer :element-array-buffer pi-buffer)
      (gl:buffer-data :element-array-buffer :static-draw position-array)
      (gl:free-gl-array position-array)
      (gl:bind-buffer :element-array-buffer 0)
      (gl:bind-buffer :element-array-buffer ni-buffer)
      (gl:buffer-data :element-array-buffer :static-draw normal-array)
      (gl:free-gl-array normal-array)
      (gl:bind-buffer :element-array-buffer 0)
      (gl:bind-buffer :element-array-buffer ti-buffer)
      (gl:buffer-data :element-array-buffer :static-draw texture-array)
      (gl:free-gl-array texture-array)
      (gl:bind-buffer :element-array-buffer 0))
    ))
|#
