#|
Initialize OpenGL, initialize window, initialize audio, initialize engine

Make a new package that facilitates handling 3D drawing and math

Note: main function code adapted from sdl2-examples "basic.lisp"

Upon further development of the 3D packages of Tamias (which will be named Canis, the genus that the wolf belongs to), there will be two "branches" mantained
the 2D branch and the 3D branch
However, I will be reorganizing and redeveloping the code base so that it's "3D only", that is, the use of SDL2 primitives is either nonexistant, or will be minimized as much as possible
The goal will be to make it so that any 2D stuff can be done through the "3D branch"
Once everything is fixed, reorganized, etc. If you want to develop 3D stuff, load Canis, if you want to develop 2D games, load Tamias
Tamias will support 3D things without needing to conenct to or load Canis specific stuff, in particular 3D particles that cast light or Shaders
|#

(ql:quickload :cl-opengl)
(ql:quickload :sdl2)
(ql:quickload :sdl2-image)
(ql:quickload :png)
;;(ql:quickload :rtg-math)


;;So, the blender pose matrix may not be applying the parent transformation


#|
Not sure if it's somewhere else in these files, but note:
Enable sharpness, hue and saturation (i.e. TV settings in the game)
|#

(defvar tamias-3d-messages nil)
(load "math-lib.cl")
(load "lib.cl")
(load "assets/plane.cl")
(load "drawing-lib.cl")

(defvar camera-x 0)
(defvar camera-y 0)
(defvar camera-z 0)

(defvar garbage-timer 0)
(defvar nika nil)
(defvar socra nil)
(defvar test-tower nil)
(defvar filling 1)
(defvar alt-nika nil)
(defvar nika-roller nil)

(defvar robert nil)

(defvar world-rotation-x 0)
(defvar world-rotation-y 0)
(defvar world-rotation-z 0)
(defvar world-rotation-inc-x 0)
(defvar world-rotation-inc-y 0)
(defvar world-rotation-inc-z 0)
(defvar world-scale 1.0)

(defvar rotating nil)
(defvar rotate-z nil)

(defvar light-r .8)
(defvar light-g .8)
(defvar light-b .8)

(defvar object-matrix (make-mat4))

(defvar move-socra? nil)
(defvar *vert-shader*
  "#version 130
void main()
{	

	// the following three lines provide the same result

	gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;
//	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
//	gl_Position = ftransform();
gl_FrontColor = gl_Color;
gl_FrontSecondaryColor = gl_SecondaryColor;
gl_BackColor = gl_Color;
gl_BackSecondaryColor = gl_SecondaryColor;

}

 ")
(defvar *frag-shader*
  "#version 130
void main()
{
	gl_FragColor = gl_Color; //vec4(0.4,0.4,0.8,1.0);
}
")

(load "importer.cl")
(defvar polygon-count 0)
#|
(setf light-r .5
      light-g .5
      light-b .5)


(defun draw (object)
  (let ((rot-x (+ world-rotation-x world-rotation-inc-x))
	(rot-y (+ world-rotation-y world-rotation-inc-y))
	(rot-z (+ world-rotation-z world-rotation-inc-z)))
    (if (> rot-x 359)
	(setf world-rotation-x 0))
    (if (> rot-y 359)
	(setf world-rotation-y 0))
    (if (> rot-z 359)
	(setf world-rotation-z 0))
    #|    (setf world-rotation-inc-x (+ world-rotation-inc-x 5))
    (setf world-rotation-inc-y (+ world-rotation-inc-y 5))|#
    (gl:rotate rot-x 1.0 0.0 0.0)
    (gl:rotate rot-y 0.0 1.0 0.0)
    (gl:rotate rot-z 0.0 0.0 1.0))
  (gl:begin :triangles)
  (loop for tri in (object-polygons object)
     do (let ((va (polygon-a tri))
	      (vb (polygon-b tri))
	      (vc (polygon-c tri)))
	  (gl:color 1.0 0.0 0.0)
	  ;;	  (gl:color r-r r-g r-b)
	  (gl:vertex (vertex-x va) (vertex-y va) (vertex-z va))
	  (gl:color 0.0 1.0 0.0)
	  (gl:vertex (vertex-x vb) (vertex-y vb) (vertex-z vb))
	  (gl:color 0.0 0.0 1.0)
	  (gl:vertex (vertex-x vc) (vertex-y vc) (vertex-z vc)))))
|#

#|
(defun draw-objs (&rest objects)
  (let ((rot-x (+ world-rotation-x world-rotation-inc-x))
	(rot-y (+ world-rotation-y world-rotation-inc-y))
	(rot-z (+ world-rotation-z world-rotation-inc-z)))
    (if (> rot-x 359)
	(setf world-rotation-x 0))
    (if (> rot-y 359)
	(setf world-rotation-y 0))
    (if (> rot-z 359)
	(setf world-rotation-z 0))
    #|    (setf world-rotation-inc-x (+ world-rotation-inc-x 5))
    (setf world-rotation-inc-y (+ world-rotation-inc-y 5))|#
    (gl:rotate rot-x 1.0 0.0 0.0)
    (gl:rotate rot-y 0.0 1.0 0.0)
    (gl:rotate rot-z 0.0 0.0 1.0)
    (loop for object in objects
       do (gl:material :front :ambient-and-diffuse (list (object-r object) (object-g object) (object-b object) 1))
	 (gl:material :front :specular '(1.0 1.0 1.0 1.0))
	 (gl:material :front :shininess 64)
	 (if (eq (length (object-vertices object)) 4)
	     (gl:material :front :emission '(0.5 .0 .0 0.1))
	     (gl:material :front :emission '(0.0 .0 .0 0.0)))
	 (gl:color (object-r object) (object-g object) (object-b object))
#|
	 (if (< (object-rotation-x object) -360)
	     (setf (object-rotation-x object) 0))
	 (if (< (object-rotation-y object) -360)
	     (setf (object-rotation-y object) 0))
	 (if (< (object-rotation-z-object) -360)
	     (setf (object-rotation-z object) 0))
	 (if (> (object-rotation-x object) 360)
	     (setf (object-rotation-x object) 0))
	 (if (> (object-rotation-y object) 360)
	     (setf (object-rotation-y object) 0))
	 (if (> (object-rotation-z-object) 360)
	     (setf (object-rotation-z object) 0))
|#
	 (gl:translate (object-x object) (object-y object) (object-z object))
	 (gl:rotate (object-rotation-x object) 1.0 0.0 0.0)
	 (gl:rotate (object-rotation-y object) 0.0 1.0 0.0)
	 (gl:rotate (object-rotation-z object) 0.0 0.0 1.0)
	 (gl:translate (- (object-x object)) (- (object-y object)) (- (object-z object)))
	 (gl:begin :triangles)
	 (loop for tri in (object-polygons object)
	    do (let ((va (polygon-a tri))
		     (vb (polygon-b tri))
		     (vc (polygon-c tri)))
		 ;;		(gl:color 1.0 0.0 0.0)
		 (gl:vertex (+ (vertex-x va) (object-x object)) (+ (vertex-y va) (object-y object)) (+ (vertex-z va) (object-z object)))
		 ;;		(gl:color 0.0 1.0 0.0)
		 (gl:vertex (+ (vertex-x vb) (object-x object)) (+ (vertex-y vb) (object-y object)) (+ (vertex-z vb) (object-z object)))
		 ;;		(gl:color 0.0 0.0 1.0)
		 (gl:vertex (+ (vertex-x vc) (object-x object)) (+ (vertex-y vc) (object-y object)) (+ (vertex-z vc) (object-z object))))
	      )
	 (gl:end)
	 (gl:load-identity)
	 (gl:translate 0.0 0.0 -50)
	 (gl:rotate rot-x 1.0 0.0 0.0)
	 (gl:rotate rot-y 0.0 1.0 0.0)
	 (gl:rotate rot-z 0.0 0.0 1.0)
	 )))
|#

(defvar world-matrix (make-mat4))
;;(setf world-matrix (make-mat4))
;;(setf (aref world-matrix 2 3) 0.0)

(defun get-offset-matrix (bones parent-index)
  (let ((ret-mat (tja-bone-bind-matrix (aref bones parent-index)))
	(inv-bind-mat (mat4-inverse (tja-bone-bind-matrix (aref bones parent-index))))
	(cur-bone (aref bones parent-index)))
;;    (setf ret-mat (mat4*mat4 ret-mat inv-bind-mat))
    (if (tja-bone-parent cur-bone)
	(setf ret-mat (mat4*mat4 ret-mat (get-offset-matrix bones (tja-bone-parent cur-bone)))))
    ret-mat))

(defun get-trans-matrix (bones frames parent-index keyframe)
  (let ((ret-mat (make-mat4))
	(frame-mat (aref frames keyframe parent-index))
	(inv-bind-mat (mat4-inverse (tja-bone-bind-matrix (aref bones parent-index))))
	(cur-bone (aref bones parent-index)))
    (setf ret-mat (mat4*mat4 inv-bind-mat frame-mat))
    (if (tja-bone-parent cur-bone)
	(setf ret-mat (mat4*mat4 ret-mat (get-trans-matrix bones frames (tja-bone-parent cur-bone) keyframe))))
    ret-mat))


(defun set-child-transform (model trans-mat child-index trans-bones)
  (let ((child-mat (aref trans-bones child-index)))
    (setf child-mat (mat4*mat4 child-mat trans-mat))
    (setf (aref trans-bones child-index) child-mat)
    (if (tja-bone-children (aref (tja-data-bones (model-animation model)) child-index))
	(loop for child in (tja-bone-children (aref (tja-data-bones (model-animation model)) child-index))
	   do (set-child-transform model child-mat child trans-bones)))))


#|


SOME WUICL: NOTES !@!!!!!

Given a transformation matrix, it is (mat4*mat4 original-mat trans-mat) I think


Ok, so, I thought I could be lazy in doing all this stuff
No. I can't. 
From Blender, I need: Bones: Head, Tail, rotation, location, scale, and everything else in between

|#


;;So calc-frame-mat here is wrong, afaik
;;I'm going to make an entire file dedicated to bones and a file for bone animation

(defun calc-frame-mat (model bone-ind trans-bones)
  (let ((inv-bind nil)
	(frame-mat nil)
	(bind-mat nil)
;;	(ident-mat (make-mat4))
;;	(parent-bind-mat (make-mat4))
;;	(parent-pose-mat (make-mat4))
;;	(offset-matrix (make-mat4));; (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) bone-ind)))
	(trans-mat (make-mat4)))
    (setf inv-bind (mat4-inverse (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) bone-ind))))
    (setf bind-mat (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) bone-ind)))
    (setf frame-mat (aref (tja-data-frames (model-animation model)) (model-key model) bone-ind))
    (if (< (model-key model) (1- (car (array-dimensions (tja-data-frames (model-animation model))))))
	(progn (setf frame-mat (mat4+mat4 frame-mat (mat4-scalar (calculate-frame-transformation frame-mat
												 (aref (tja-data-frames (model-animation model)) (1+ (model-key model)) bone-ind)
												 (aref (tja-data-timing (model-animation model)) (model-key model))
												 (aref (tja-data-timing (model-animation model)) (1+ (model-key model))))
								 (- (model-frame model) (aref (tja-data-timing (model-animation model)) (model-key model))))))
	       (if (> (model-frame model) (aref (tja-data-timing (model-animation model)) (1+ (model-key model))))
		   (incf (model-key model))))
	(if (>= (model-frame model) (aref (tja-data-timing (model-animation model)) (model-key model)))
	    (setf (model-key model) 0
		  (model-frame model) 1.0)))
;;    (if (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind))
;;	(setf parent-bind-mat (mat4-inverse (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind)))))
;;	      parent-pose-mat (mat4-inverse (aref (tja-data-frames (model-animation model)) (1+ (model-key model)) (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind))))))
;;    (setf frame-mat (mat4*mat4 frame-mat (aref trans-bones bone-ind)))
#|    (if (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind))
	(progn (setf offset-matrix (get-offset-matrix (tja-data-bones (model-animation model))
						      bone-ind)
		     trans-mat (get-trans-matrix (tja-data-bones (model-animation model))
						 (tja-data-frames (model-animation model))
						 bone-ind
						 (model-key model)))))
    (if (tja-bone-children (aref (tja-data-bones (model-animation model)) bone-ind))
	(loop for child in (tja-bone-children (aref (tja-data-bones (model-animation model)) bone-ind))
	     do (set-child-transform model (mat4*mat4 frame-mat inv-bind) child trans-bones)))
    (setf offset-matrix (mat4-inverse offset-matrix))|#
    #|
    So, what I may have to do is go through the parent of each parent bone, get their transform matrices, and apply their transforms to the current bone
    I'm not 100% sure as of right now, but that's something to look into
|#
;;    (setf trans-mat (mat4*mat4 t-mat frame-mat))
;;    (setf trans-mat (mat4*mat4 parent-mat trans-mat))
;;    (mat4*mat4 t-mat frame-mat)
;;    (mat4*mat4 t-mat (mat4*mat4 inv-bind frame-mat))
    ;;    (mat4*mat4 t-mat trans-mat)
    ;;    (mat4*mat4 offset-matrix (mat4*mat4 frame-mat inv-bind))
    (setf trans-mat frame-mat)
;;    (setf trans-mat (mat4*mat4 (mat4*mat4 frame-mat parent-pose-mat) (mat4-inverse (mat4*mat4 bind-mat parent-bind-mat))))
    (setf trans-mat (mat4*mat4 world-matrix (mat4*mat4 trans-mat inv-bind)))
;;    (mat4*mat4 trans-mat offset-matrix)
;;    offset-matrix
;;    (mat4*mat4 frame-mat inv-bind)
;;    (mat4*mat4 trans-mat offset-matrix)
;;    (mat4*mat4 offset-matrix trans-mat)
    ))

(defvar obj-mat (make-mat4))

(defun calc-frame-vals (model pos-ind vec trans-bones)
  (let ((return-vec (make-array '(3)))
	(tmp-vec (make-array '(3)))
	(trans-mat (make-mat4))
	(normalized-weight 0.0))
    (setf (aref trans-mat 0 0) 0.0
	  (aref trans-mat 1 1) 0.0
	  (aref trans-mat 2 2) 0.0
	  (aref trans-mat 3 3) 0.0)
    (loop for weight in (aref (model-weights model) pos-ind)
       do (incf normalized-weight (cadr weight)))
    (setf normalized-weight (/ 1.0 normalized-weight))
    (if (< normalized-weight 1.0)
	(setf normalized-weight 1.0))
    (loop for weight in (reverse (aref (model-weights model) pos-ind))
       do (let ((weighted-mat (aref trans-bones (car weight))))
;;	    (mat4-scalar (aref trans-bones (car weight)) (* (cadr weight) normalized-weight))))
	    (setf tmp-vec (mat4*vec4 weighted-mat vec))
	    (setf tmp-vec (vec3-scalar tmp-vec (* (cadr weight) normalized-weight)))
	    (setf return-vec (vec3+vec3 return-vec tmp-vec))))
;;	    (setf trans-mat (mat4+mat4 trans-mat (mat4-scalar weighted-mat normalized-weight)))))
;;    (setf return-vec (mat4*vec4 trans-mat vec))
    return-vec
    ))

;;(setf (model-key nika) 0)

(defvar nika-timer 0)

(defvar nika-bent-jump nil)

(defvar bones-printed? nil)

;;(incf (model-z nika-roller) -3)

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


(setf bones-printed? nil)

(defvar anim-running? t)
(setf anim-running? nil)
(defun test-opengl-draw ()
  (if rotating
      (progn (incf world-rotation-inc-x .5)
	     (incf world-rotation-inc-z .5)))
  (if rotate-z
      (incf world-rotation-inc-y .6))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((asp (/ 900 1200.0))) ;;asp = width/height
    (gl:frustum -1.0 1.0 (- asp) asp (+ 1.0 world-scale) 10000.0))
;;    (gl:ortho (+ -400 world-scale) (+ 400 world-scale) (+ -400 world-scale) (+ 400 world-scale) (+ -800 world-scale) (+ 800 world-scale)))
  ;;(gl:frustum LEFT RIGHT NEG-ASPect ASPect ZNEAR VIEW-DISTANCE)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;;  (gl:translate 0.0 -50.0 -300)
  (gl:translate (+ 0.0 camera-x) (+ -50.0 camera-y) (+ -300 camera-z))
  #|
  (incf (object-rotation-y diamond) .1)
  (incf (object-rotation-y cube) .1)
  (incf (object-rotation-z cube) .1)
  (incf (object-rotation-z sphere) .2)
  (draw-objs plane diamond cube sphere))
  (decf (object-rotation-y (car (model-objects nika))) .5)
  (decf (object-rotation-y (cadr (model-objects nika))) .5)
  (decf (object-rotation-y (caddr (model-objects nika))) .5)
  (decf (object-rotation-y (cadddr (model-objects nika))) .5)
  |#
  ;;  (incf (model-rotation-y test-tower) 1.0)
;;  (incf nika-timer)
#|  (if (>= nika-timer 60)
      (progn (incf (model-key nika))
	     (setf nika-timer 0)))
  (if (> (model-key nika) 5)
  (setf (model-key nika) 0))|#
#|  (if anim-running?
      (progn (incf (model-frame nika-bent-jump) .5)
	     (incf (model-frame robert) .5)
	     (incf (model-frame nika-roller) .5)))
  |#(alt-draw plane;; socra test-tower dress-plane
;;	    nika
	    ;;	    alt-nika
;;	    robert
;;	    nika-bent-jump
;;	    nika-roller
	    ))

(defvar model-key 0)
#|
(defun set-nika-values ()
  (setf (model-scale-scalar alt-nika) 10.0)
  (setf model-key 0
	(model-key alt-nika) model-key
	(model-key nika) model-key
	(model-key nika-roller) model-key)
  
  (setf (model-x alt-nika) 20.0
	(model-x nika-roller) -20.0
	(model-x nika) -20.0)
  
  (setf (model-z test-tower) 90)
  (setf (model-scale-scalar test-tower) 4.0)
  (setf (model-rotation-x test-tower) 0.0)
  (setf (model-x socra) 60)
  (setf (model-key nika) 0))
|#
;;(set-nika-values)

(defun set-key-frames (models)
  (loop for model in models
     do (incf (model-key model))
       (if (> (model-key model) (1- (car (array-dimensions (tja-data-frames (model-animation model))))))
	   (setf (model-key model) 0))))

(defun key-input (key)
  (cond ((sdl2:scancode= key :scancode-space)
	 (setf world-rotation-inc-x 0)
	 (setf world-rotation-inc-y 0)
	 (setf world-rotation-inc-z 0)
	 )
	((sdl2:scancode= key :scancode-3)
	 (set-key-frames (list nika-bent-jump nika-roller)))
	((sdl2:scancode= key :scancode-s)
	 (setf rotating (not rotating)))
	((sdl2:scancode= key :scancode-w)
	 (if (eq filling 0)
	     (progn (gl:polygon-mode :front-and-back :fill)
		    (setf filling 1))
	     (progn (gl:polygon-mode :front-and-back :line)
		    (setf filling 0))))
	((sdl2:scancode= key :scancode-a)
	 (if (or (eq filling 1)
		 (eq filling 0))
	     (progn (gl:polygon-mode :front-and-back :point)
		    (setf filling 2))
	     (progn (gl:polygon-mode :front-and-back :fill)
		    (setf filling 1))))
	((sdl2:scancode= key :scancode-b)
	 (incf camera-x 1))
	((sdl2:scancode= key :scancode-1)
	 (setf camera-x 0
	       camera-y 0
	       camera-z 0))
	((sdl2:scancode= key :scancode-p)
	 (setf anim-running? (not anim-running?)))
	((sdl2:scancode= key :scancode-2)
	 (incf camera-y 30))
	((sdl2:scancode= key :scancode-g)
	 (setf move-socra? (not move-socra?)))
	((sdl2:scancode= key :scancode-m)
	 (setf world-rotation-inc-x (+ world-rotation-inc-x 1)))
	((sdl2:scancode= key :scancode-i)
	 (setf world-rotation-inc-x (- world-rotation-inc-x 1)))
	((sdl2:scancode= key :scancode-j)
	 (setf world-rotation-inc-y (+ world-rotation-inc-y 1)))
	((sdl2:scancode= key :scancode-l)
	 (setf world-rotation-inc-y (- world-rotation-inc-y 1)))
	((sdl2:scancode= key :scancode-z)
	 (setf world-rotation-inc-z (- world-rotation-inc-z 5)))
	((sdl2:scancode= key :scancode-x)
	 (setf world-rotation-inc-z (+ world-rotation-inc-z 5)))
	((sdl2:scancode= key :scancode-r)
	 (setf rotate-z (not rotate-z)))
	))

(defun main ()
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	#|        (let ((controllers ())
	(haptic ()))|#
	;; basic window/gl setup
	(format t "Setting up window/gl.~%")
	(finish-output)
	(sdl2:gl-make-current win gl-context)
	(sdl2:set-window-size win 1200 900)
	(gl:viewport 0 0 1200 900)
	(gl:shade-model :smooth)
	(gl:matrix-mode :projection)
	(gl:load-identity)
	(gl:ortho -100 100 -100 100 -200 200)
	(gl:cull-face :back)
	(gl:light :light0 :position '(0.0 0.0 -50.0 1.0))
	(gl:light :light0 :diffuse (vector light-r light-g light-b 1))
;;	(gl:light-model :light-model-ambient '(.5 .5 .5 1.0))
;;	(gl:light-model :light-model-local-viewer 1)
	(gl:enable :normalize :depth-test :lighting :light0)
	(gl:enable :texture-2d)
	(gl:enable :blend)
;;	(gl:disable :dither)
	(gl:polygon-mode :front-and-back :fill)
	(gl:matrix-mode :modelview)
	(gl:load-identity)
	(gl:clear-color 0.3 0.3 0.3 1.0)
	(gl:clear :color-buffer :depth-buffer) 



	#|          (format t "Opening game controllers.~%")
	(finish-output)
	;; open any game controllers
	(loop :for i :upto (- (sdl2:joystick-count) 1)
	:do (when (sdl2:game-controller-p i)
	(format t "Found gamecontroller: ~a~%"
	(sdl2:game-controller-name-for-index i))
	(let* ((gc (sdl2:game-controller-open i))
	(joy (sdl2:game-controller-get-joystick gc)))
	(setf controllers (acons i gc controllers))
	(when (sdl2:joystick-is-haptic-p joy)
	(let ((h (sdl2:haptic-open-from-joystick joy)))
	(setf haptic (acons i h haptic))
	(sdl2:rumble-init h))))))|#

	;; main loop
	(format t "Beginning main loop.~%")
	(finish-output)
	(let ((vs (gl:create-shader :vertex-shader))
	      (fs (gl:create-shader :fragment-shader))
	      (shader-prog nil))
	  
	  ;;	    (setf (vertex-shader w) vs)
	  ;;	    (setf (fragment-shader w) fs)
	  (gl:shader-source vs *vert-shader*)
	  (gl:compile-shader vs)
	  (gl:shader-source fs *frag-shader*)
	  (gl:compile-shader fs)
	  ;; If the shader doesn't compile, you can print errors with:
	  (print (gl:get-shader-info-log vs))
	  (print (gl:get-shader-info-log fs))
	  
	  (setf shader-prog (gl:create-program))
	  ;; You can attach the same shader to multiple different programs.
	  (gl:attach-shader shader-prog vs)
	  (gl:attach-shader shader-prog fs)
	  ;; Don't forget to link the program after attaching the
	  ;; shaders. This step actually puts the attached shader together
	  ;; to form the program.
	  #|	    (gl:link-program shader-prog)
	  ;; If we want to render using this program object, or add
	  ;; uniforms, we need to use the program. This is similar to
	  ;; binding a buffer.
	  (gl:use-program shader-prog)|#
	  ;;	  (sdl2:hide-cursor)
	  ;;	  (setf (object-texture-id (cadr (model-objects nika))) (load-png #P"/home/neon-frost/Work Files/3D/Nika Dress Tex - Dress Patterns.png")
#|	  (setf (object-texture-id (cadr (model-objects nika))) (load-texture #P"/home/neon-frost/Work Files/3D/Nika Dress Tex - Dress Patterns.png")
		(object-texture-id (car (model-objects dress-plane))) (object-texture-id (cadr (model-objects nika)))
	  (object-texture-id (car (model-objects test-tower))) (load-texture "/home/neon-frost/Work Files/3D/Cavern Floor - Cartoon.png"))|#
	  #|
	  (if (not (model-animation nika))
	      (progn (setf (model-animation nika) (tja-importer "Nika.tja")
			   (model-weights nika) (make-array (list (car (array-dimensions (model-vertices nika))))))
		     (loop for n below (car (array-dimensions (tja-data-weights (model-animation nika))))
			do (let ((weight-arr (aref (tja-data-weights (model-animation nika)) n)))
			     (if (eq (aref (model-weights nika) (aref weight-arr 0)) 0)
				 (progn (setf (aref (model-weights nika) (aref weight-arr 0)) nil
					      (aref (model-weights nika) (aref weight-arr 0)) (remove nil (aref (model-weights nika) (aref weight-arr 0))))
					(push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights nika) (aref weight-arr 0))))
				 (push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights nika) (aref weight-arr 0))))))))
	  (if (not alt-nika)
	      (progn (setf alt-nika (t3da-importer "Nika.t3da")
			   (model-weights alt-nika) (make-array (list (car (array-dimensions (model-vertices alt-nika))))))
		     (loop for n below (car (array-dimensions (tja-data-weights (model-animation alt-nika))))
			do (let ((weight-arr (aref (tja-data-weights (model-animation alt-nika)) n)))
			     (if (eq (aref (model-weights alt-nika) (aref weight-arr 0)) 0)
				 (progn (setf (aref (model-weights alt-nika) (aref weight-arr 0)) nil
					      (aref (model-weights alt-nika) (aref weight-arr 0)) (remove nil (aref (model-weights alt-nika) (aref weight-arr 0))))
					(push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights alt-nika) (aref weight-arr 0))))
				 (push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights alt-nika) (aref weight-arr 0))))))))
	  (if (not nika-roller)
	      (progn (setf nika-roller (t3da-importer "Nika_roll.t3da")
			   (model-weights nika-roller) (make-array (list (car (array-dimensions (model-vertices nika-roller))))))
		     (loop for n below (car (array-dimensions (tja-data-weights (model-animation nika-roller))))
			do (let ((weight-arr (aref (tja-data-weights (model-animation nika-roller)) n)))
			     (if (eq (aref (model-weights nika-roller) (aref weight-arr 0)) 0)
				 (progn (setf (aref (model-weights nika-roller) (aref weight-arr 0)) nil
					      (aref (model-weights nika-roller) (aref weight-arr 0)) (remove nil (aref (model-weights nika-roller) (aref weight-arr 0))))
					(push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights nika-roller) (aref weight-arr 0))))
				 (push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights nika-roller) (aref weight-arr 0))))))))
	  (if (not nika-bent-jump)
	      (progn (setf nika-bent-jump (t3da-importer "Nika_bent_side.t3da")
			   (model-weights nika-bent-jump) (make-array (list (car (array-dimensions (model-vertices nika-bent-jump))))))
		     (loop for n below (car (array-dimensions (tja-data-weights (model-animation nika-bent-jump))))
			do (let ((weight-arr (aref (tja-data-weights (model-animation nika-bent-jump)) n)))
			     (if (eq (aref (model-weights nika-bent-jump) (aref weight-arr 0)) 0)
				 (progn (setf (aref (model-weights nika-bent-jump) (aref weight-arr 0)) nil
					      (aref (model-weights nika-bent-jump) (aref weight-arr 0)) (remove nil (aref (model-weights nika-bent-jump) (aref weight-arr 0))))
					(push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights nika-bent-jump) (aref weight-arr 0))))
				 (push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights nika-bent-jump) (aref weight-arr 0))))))))
	  (if (not robert)
	      (progn (setf robert (t3da-importer "Robert.t3da")
			   (model-weights robert) (make-array (list (car (array-dimensions (model-vertices robert))))))
		     (loop for n below (car (array-dimensions (tja-data-weights (model-animation robert))))
			do (let ((weight-arr (aref (tja-data-weights (model-animation robert)) n)))
			     (if (eq (aref (model-weights robert) (aref weight-arr 0)) 0)
				 (progn (setf (aref (model-weights robert) (aref weight-arr 0)) nil
					      (aref (model-weights robert) (aref weight-arr 0)) (remove nil (aref (model-weights robert) (aref weight-arr 0))))
					(push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights robert) (aref weight-arr 0))))
				 (push (list (aref weight-arr 1) (aref weight-arr 2)) (aref (model-weights robert) (aref weight-arr 0))))))))

	  (setf (model-x alt-nika) 20.0
		(model-x nika-bent-jump) 20.0
		(model-x nika-roller) -20.0
		(model-z robert) -100
		(model-x nika) -20.0)
	  |#
;;	  (setf nika-dress-id (load-png #P"/home/neon-frost/Work Files/3D/Nika Dress Tex - Dress Patterns.png"))
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
                      (let ((scancode (sdl2:scancode-value keysym))
                            (sym (sdl2:sym-value keysym))
                            (mod-value (sdl2:mod-value keysym)))
			(key-input scancode)
			#|                        (cond
			((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
			((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
			((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
			
                        (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                                sym
                                scancode
                                mod-value)))|#
			))
            (:keyup (:keysym keysym)
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)))

            (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
			  (if (eq state 4)
			      (incf world-scale (/ yrel -10.0)))
			  (if (< world-scale 0.05)
			      (setf world-scale 0.05))
			  (if (eq state 2)
			      (progn (setf world-rotation-inc-x (+ world-rotation-inc-x yrel))
				     (setf world-rotation-inc-y (+ world-rotation-inc-y xrel))))
			  (if move-socra?
			      (incf (model-x socra) (/ xrel 5.0)))
#|                          (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
			  x xrel y yrel state))|#
			  )
	    #|            (:controlleraxismotion
	    (:which controller-id :axis axis-id :value value)
	    (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
	    controller-id axis-id value))

            (:controllerbuttondown (:which controller-id)
	    (let ((h (cdr (assoc controller-id haptic))))
	    (when h
	    (sdl2:rumble-play h 1.0 100))))
	    |#
            (:idle ()
                   (gl:clear :color-buffer :depth-buffer)
		   (test-opengl-draw)
                   (gl:flush)
                   (sdl2:gl-swap-window win)
		   (incf garbage-timer)
		   (if (> garbage-timer 1000)
		       (progn (setf garbage-timer 0)
			      (gc :full t)))
		   )
            (:quit ()
		   (gl:delete-shader vs)
		   (gl:delete-shader fs)
		   (gl:delete-program shader-prog)
		   t)))
	(format t "Closing opened game controllers.~%")
	(finish-output)))))

;; close any game controllers that were opened as well as any haptics
#|          (loop :for (i . controller) :in controllers
:do (sdl2:game-controller-close controller)
(sdl2:haptic-close (cdr (assoc i haptic)))))))))
|#

#|
(require :lispbuilder-sdl)
(require :cl-opengl)

;;;;3bgl-shader is the Common LISP DSL for generating shaders

(defun test ()
  (sdl:with-init ()
    (sdl:window 320 240
		:title-caption "OpenGL example"
		:opengl T
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl:clear-color 0 0 0 0)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    (sdl:with-events ()
      (:quit-event () t)
      (WHEN (SDL:KEY= KEY :SDL-KEY-ESCAPE)
	(SDL:PUSH-QUIT-EVENT))
      (:idle ()
	     (gl:clear :color-buffer-bit)
	     (gl:color 1 1 1)
	     (gl:with-primitive :polygon
	       (gl:vertex 0.25 0.25 0.0)
	       (gl:vertex 0.75 0.25 0.0)
	       (gl:vertex 0.75 0.75 0.0)
	       (gl:vertex 0.25 0.75 0.0))
	     (gl:flush)
	     (sdl:update-display)
	     ))))

(defvar filling 1)
(defvar rotation-x 0)
(defvar rotation-y 0)
(defvar rotation-z 0)
(defvar rotation-inc-x 0)
(defvar rotation-inc-y 0)
(defvar rotation-inc-z 0)

(defun test-triangle ()
  (sdl:with-init ()
    (sdl:window 1040 700
		:title-caption "Triangle Test"
		:opengl T
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl-init 1040 700)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (key-input key))
					;      (WHEN (SDL:KEY= KEY :SDL-KEY-ESCAPE) ;
					;	(SDL:PUSH-QUIT-EVENT)) ;
      (:idle ()
	     #|	     (gl:clear :color-buffer-bit)
	     (gl:color 1 1 1)
	     (gl:with-primitive :polygon
	     (gl:vertex 0.25 0.25 0.0)
	     (gl:vertex 0.75 0.25 0.0)
	     (gl:vertex 0.75 0.75 0.0)
	     (gl:vertex 0.25 0.75 0.0))
	     (gl:flush)|#
	     (draw triangle)
	     (sdl:update-display)
	     (incf rotation-inc-y 10)
	     ))))

(defun test-cube ()
  (sdl:with-init ()
    (sdl:window 640 480
		:title-caption "Cube Test"
		:opengl T
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl-init 640 480)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (key-input key))
					;      (WHEN (SDL:KEY= KEY :SDL-KEY-ESCAPE) ;
					;	(SDL:PUSH-QUIT-EVENT)) ;
      (:idle ()
	     #|	     (gl:clear :color-buffer-bit)
	     (gl:color 1 1 1)
	     (gl:with-primitive :polygon
	     (gl:vertex 0.25 0.25 0.0)
	     (gl:vertex 0.75 0.25 0.0)
	     (gl:vertex 0.75 0.75 0.0)
	     (gl:vertex 0.25 0.75 0.0))
	     (gl:flush)|#
	     (draw cube)
	     (sdl:update-display)
	     ))))

(defun test-pyramid ()
  (sdl:with-init ()
    (sdl:window 960 720
		:title-caption "Pyramid Test"
		:opengl T
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl-init 960 720)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (key-input key))
					;      (WHEN (SDL:KEY= KEY :SDL-KEY-ESCAPE) ;
					;	(SDL:PUSH-QUIT-EVENT)) ;
      (:idle ()
	     #|	     (gl:clear :color-buffer-bit)
	     (gl:color 1 1 1)
	     (gl:with-primitive :polygon
	     (gl:vertex 0.25 0.25 0.0)
	     (gl:vertex 0.75 0.25 0.0)
	     (gl:vertex 0.75 0.75 0.0)
	     (gl:vertex 0.25 0.75 0.0))
	     (gl:flush)|#
	     (draw pyramid)
	     (sdl:update-display)
	     (incf rotation-inc-x 5)
	     ))))

(defun test-diamond ()
  (sdl:with-init ()
    (sdl:window 960 720
		:title-caption "Diamond Test"
		:opengl T
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl-init 960 720)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (key-input key))
					;      (WHEN (SDL:KEY= KEY :SDL-KEY-ESCAPE) ;
					;	(SDL:PUSH-QUIT-EVENT)) ;
      (:idle ()
	     #|	     (gl:clear :color-buffer-bit)
	     (gl:color 1 1 1)
	     (gl:with-primitive :polygon
	     (gl:vertex 0.25 0.25 0.0)
	     (gl:vertex 0.75 0.25 0.0)
	     (gl:vertex 0.75 0.75 0.0)
	     (gl:vertex 0.25 0.75 0.0))
	     (gl:flush)|#
	     (draw diamond)
	     (sdl:update-display)
	     (incf rotation-inc-x 5)
	     ))))



(defun gl-init (width height)
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :smooth)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -100 100 -100 100 -100 100)
  (gl:enable :depth-test)
  (gl:polygon-mode :front-and-back :fill)
  )

(defun key-input (key)
  (case key
    (:SDL-KEY-ESCAPE (SDL:PUSH-QUIT-EVENT))
    (:sdl-key-space
     (setf rotation-inc-x 0)
     (setf rotation-inc-y 0)
     (setf rotation-inc-z 0)
     )
    (:sdl-key-r
     (if (eq filling 0)
	 (progn (gl:polygon-mode :front-and-back :fill)
		(setf filling 1))
	 (progn (gl:polygon-mode :front-and-back :line)
		(setf filling 0))))
    (:sdl-key-up
     (setf rotation-inc-x (+ rotation-inc-x 5)))
    (:sdl-key-down
     (setf rotation-inc-x (- rotation-inc-x 5)))
    (:sdl-key-left
     (setf rotation-inc-y (+ rotation-inc-y 5)))
    (:sdl-key-right
     (setf rotation-inc-y (- rotation-inc-y 5)))
    )
  )

(defun draw (object)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate 0.0 0.0 -50)
  (let ((rot-x (+ rotation-x rotation-inc-x))
	(rot-y (+ rotation-y rotation-inc-y))
	(rot-z (+ rotation-z rotation-inc-z)))
    (if (> rot-x 359)
	(setf rotation-x 0))
    (if (> rot-y 359)
	(setf rotation-y 0))
    (if (> rot-z 359)
	(setf rotation-z 0))
    #|    (setf rotation-inc-x (+ rotation-inc-x 5))
    (setf rotation-inc-y (+ rotation-inc-y 5))|#
    (gl:rotate rot-x 1.0 0.0 0.0)
    (gl:rotate rot-y 0.0 1.0 0.0)
    (gl:rotate rot-z 0.0 0.0 1.0)
    )
  (gl:begin :triangles)
  (loop for tri in (object-polygons object)
     do (let ((va (polygon-a tri))
	      (vb (polygon-b tri))
	      (vc (polygon-c tri)))
	  (gl:color 1.0 0.0 0.0)
	  (gl:vertex (vertex-x va) (vertex-y va) (vertex-z va))
	  (gl:color 0.0 1.0 0.0)
	  (gl:vertex (vertex-x vb) (vertex-y vb) (vertex-z vb))
	  (gl:color 0.0 0.0 1.0)
	  (gl:vertex (vertex-x vc) (vertex-y vc) (vertex-z vc))))
  (gl:end)
  (gl:flush)
  )

|#
(defun print-messages ()
  (loop for msg in (reverse tamias-3d-messages)
     do (princ msg)
       (fresh-line)))
(print-messages)

