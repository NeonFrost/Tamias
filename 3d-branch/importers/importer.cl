#|
Skeleton File to import assets of {format}

Oh jeez, oh god, oh fuck

So if you're reading this, I apologize if trying to use this engine is difficult, hellish, or otherwise headache inducing.
I have no idea what I'm doing, there's this thing called a jacobian matrix which is a partial differentiation matrix or some shit like that
Something about limits, derivatives and skeletal animation
I failed calculus twice for fuck's sake, I was educated in one of the 5 worst states for education. The highest I've seen it ranked was 46.
I've sustained permanent nerve damage from my last job (sanitation in a local factory), I lost my ability to do higher math for 7 years
resulting in having difficulty in doing even simple addition (for about a year I could barely manage something like 28 + 27)
and it's only been in the last year (as of 9/23/19) that I've been able to do multi-digit multiplication in my head again.

So, I apologize if this is difficult to use. I am open to suggestions for how to make things be a bit more understandable and easier to use
so long as they don't radically alter how I've designed the engine

|#

#|
(defpackage tamias-importer
  )

(in-package :tamias-importer)



Structure of an obj file:

# program source
# website
material-library name
object name
spatial-vertices x y z
texture-positions u v
vertex-normals x y z
use-material material-name
smooth-shading 1/off
faces spatial-vert-1/texture-vert-1/normal-vert-1  ... sv3/tv3/nv3
object name
|#    


(defun parse-obj-vertex (ip-str)
  (let* ((spat-vert (make-array '(3) :initial-contents '(0.0 0.0 0.0)))
	 (sv-verts (subseq ip-str 2))
	 (sv-x (subseq sv-verts 0 (position #\space sv-verts)))
	 (sv-y (subseq sv-verts (1+ (length sv-x)) (position #\space sv-verts :start (1+ (length sv-x)))))
	 (sv-z (subseq sv-verts (+ (length sv-x) (length sv-y) 2))))
    (setf (aref spat-vert 0) (parse-float sv-x)
	  (aref spat-vert 1) (parse-float sv-y)
	  (aref spat-vert 2) (parse-float sv-z))
    spat-vert))

(defun parse-obj-uv (ip-str)
  (let* ((tex-vert (make-array '(2) :initial-contents '(0.0 0.0)))
	 (tv-verts (subseq ip-str 3))
	 (tv-u (subseq tv-verts 0 (position #\space tv-verts)))
	 (tv-v (subseq tv-verts (1+ (position #\space tv-verts)))))
    (setf (aref tex-vert 0) (parse-float tv-u)
	  (aref tex-vert 1) (parse-float tv-v))
    tex-vert))

(defun parse-obj-normal (ip-str)
  (let* ((normal-vert (make-array '(3) :initial-contents '(0.0 0.0 0.0)))
	 (nv-verts (subseq ip-str 3))
	 (nv-x (subseq nv-verts 0 (position #\space nv-verts)))
	 (nv-y (subseq nv-verts (1+ (length nv-x)) (position #\space nv-verts :start (1+ (length nv-x)))))
	 (nv-z (subseq nv-verts (+ (length nv-x) (length nv-y) 2))))
    (setf (aref normal-vert 0) (parse-float nv-x)
	  (aref normal-vert 1) (parse-float nv-y)
	  (aref normal-vert 2) (parse-float nv-z))
    normal-vert))

(defun parse-obj-face (ip-str &key (obj? t))
  (let* ((points-string (subseq ip-str (1+ (position #\space ip-str)))) ;;testing string: "104/262/200 141/216/197 326/263/198"
	 (polygon nil)
	 (points nil)
	 (current-point nil))
    (loop while t
       do (let ((current-point-string (subseq points-string
					      0
					      (or (position #\space points-string)
						  nil)))
		(current-vert nil)
		(verts nil))
	    (if (find #\space points-string)
		(setf points-string (subseq points-string
					    (1+ (position #\space points-string))))
		(setf points-string ""))
	    (setf current-point (make-point))
	    (if (find #\/ current-point-string)
		(progn (loop while t
			  do (setf current-vert (subseq current-point-string 0 (or (position #\/ current-point-string)
										   (length current-point-string))))
			    (if (find #\/ current-point-string)
				(setf current-point-string (subseq current-point-string (1+ (position #\/ current-point-string))))
				(setf current-point-string ""))
			    (if (> (length current-vert) 0)
				(if obj?
				    (push (1- (parse-integer current-vert)) verts)
				    (push (parse-integer current-vert) verts))
				(push 'nil verts))
			    (if (eq (length current-point-string) 0)
				(return t)))
		       (setf (point-normal-index current-point) (car verts)
			     (point-texture-index current-point) (cadr verts)
			     (point-position-index current-point) (caddr verts)))
		(setf (point-position-index current-point) (parse-integer current-point-string)))
	    (push current-point points)
	    (if (eq (length points-string) 0)
		(return t))))
    (setf polygon (make-polygon :points (make-array (list (length points)) :initial-contents (reverse points))))
    polygon))

(defun load-obj (file-name)
  (let ((objs nil)
	(model (make-model))
	(model-info nil)
	(material-library nil)
	(current-object nil)
	(object-name nil)
	(spatial-verts nil)
	(texture-verts nil)
	(normal-verts nil)
	(object-material nil)
	(smoothing nil)
	(polygons nil))
    (with-open-file (ip-stream file-name)
      (loop for current-line = (read-line ip-stream nil)
	 do (if (not current-line)
		(return t))
	 ;;	   (princ current-line) ;;debug
	   (cond ((char= (aref current-line 0) #\#)
		  (push (subseq current-line 1) model-info))
		 ((char= (aref current-line 0) #\s)
		  (if (char= (aref current-line 2) #\1)
		      (setf smoothing t)
		      (setf smoothing nil)))
		 ((char= (aref current-line 0) #\m)
		  (setf material-library (subseq current-line (position #\space current-line))))
		 ((char= (aref current-line 0) #\v)
		  (cond ((char= (aref current-line 1) #\space)
			 ;;spatial verts
			 (push (parse-obj-vertex current-line) spatial-verts))
			((char= (aref current-line 1) #\t)
			 ;;texture verts
			 (push (parse-obj-uv current-line) texture-verts))
			((char= (aref current-line 1) #\n)
			 ;;normal verts
			 (push (parse-obj-normal current-line) normal-verts))))
		 ((char= (aref current-line 0) #\u)
		  (setf object-material (subseq current-line (position #\space current-line)))
		  (fresh-line))
		 ((char= (aref current-line 0) #\f)
		  (push (parse-obj-face current-line) polygons))
		 ((char= (aref current-line 0) #\o)
		  (if model-info
		      (if (listp model-info)
			  (let ((obj-info ""))
			    (loop for info in (reverse model-info)
			       do (setf obj-info (concatenate 'string obj-info info '(#\newline))))
			    (setf model-info obj-info))))
		  (if current-object
		      (progn (setf current-object (make-object :name object-name
							       :info model-info
							       :material object-material
							       :smoothing smoothing
							       :polygons (make-array (length polygons) :initial-contents (reverse polygons))))
			     (push current-object objs)
			     (setf current-object nil
				   object-material nil
				   object-name nil
				   smoothing nil
				   polygons nil)))
		  (setf current-object (make-object)
			object-name (subseq current-line (position #\space current-line)))))))
    (setf current-object (make-object :name object-name
				      :info model-info
				      :material object-material
				      :smoothing smoothing
				      :polygons (make-array (length polygons) :initial-contents (reverse polygons))))
    (push current-object objs)
    (let ((vertices (make-array (list (length spatial-verts))))
	  (normal-vertices (make-array (list (length normal-verts))))
	  (texture-vertices (make-array (list (length texture-verts))))
	  (spatial-verts (reverse spatial-verts))
	  (normal-verts (reverse normal-verts))
	  (texture-verts (reverse texture-verts)))
      (loop for index below (length spatial-verts)
	 for spat-vert in spatial-verts
	 do (setf (aref vertices index) (make-vertex))
	   (setf (vertex-values (aref vertices index)) spat-vert))
      (loop for index below (length normal-verts)
	 for norm-vert in normal-verts
	 do (setf (aref normal-vertices index) (make-vertex))
	   (setf (vertex-values (aref normal-vertices index)) norm-vert))
      (loop for index below (length texture-verts)
	 for tex-vert in texture-verts
	 do (setf (aref texture-vertices index) (make-vertex))
	   (setf (vertex-values (aref texture-vertices index)) tex-vert))
      (setf model (make-model :vertices vertices
			      :normal-vertices normal-vertices
			      :texture-vertices texture-vertices
			      :objects (reverse objs)))
      model)))

(defun load-nika ()
  (setf nika (load-obj "/home/neon-frost/Work Files/3D/Nika.obj"))
  (loop for obj in (model-objects nika)
     do (setf (object-width obj) 15.0
	      (object-height obj) 15.0
	      (object-depth obj) 15.0))
  ;;scarf -> dress -> eys -> body
  (setf (object-r (car (model-objects nika))) 0.8
	(object-g (car (model-objects nika))) 0.0
	(object-b (car (model-objects nika))) 0.0
	(object-r (cadr (model-objects nika))) 0.0
	(object-g (cadr (model-objects nika))) 0.23
	(object-b (cadr (model-objects nika))) 0.24
	(object-r (caddr (model-objects nika))) 0.763
	(object-g (caddr (model-objects nika))) 0.8
	(object-b (caddr (model-objects nika))) 0.0
	(object-r (cadddr (model-objects nika))) 0.013
	(object-g (cadddr (model-objects nika))) 0.0
	(object-b (cadddr (model-objects nika))) 0.035)
  (setf socra (load-obj "/home/neon-frost/Work Files/3D/Socra.obj"))
  (loop for obj in (model-objects socra)
     do (setf (object-width obj) 15.0
	      (object-height obj) 15.0
	      (object-depth obj) 15.0
	      (object-x obj) 5.0
	      (object-z obj) 5.0))
  (setf test-tower (load-obj "/home/neon-frost/Work Files/3D/Tower Test.obj"))
  (loop for obj in (model-objects test-tower)
     do (setf (object-width obj) 15.0
	      (object-height obj) 15.0
	      (object-depth obj) 15.0
	      (object-r obj) 1.0
	      (object-b obj) 1.0
	      (object-g obj) 1.0
	      (object-z obj) -200.0)))
	
;;(load-nika)

#|
(defun load-png (filename &optional (texture-id (car (gl:gen-textures 1))
                                                 texture-id-p) )
  (flet ((load-and-decode (filename)
           (with-open-file (in filename
                               :element-type '(unsigned-byte 8))
             (png:decode in))))
    (handler-case
        (let ((png (load-and-decode filename)))
          (assert png)          ; make sure we got the png
          (gl:bind-texture :texture-2d texture-id)
	  (let ((ww (png:image-width png))
                (hh (png:image-height png))
                (cc (png:image-channels png)))
            (let ((data (make-array (list (* ww hh cc))
                                    :element-type (array-element-type png)
                                    :displaced-to png)))
              (let ((level-of-detail 0)
                    (internal-format (ecase (png:image-bit-depth png)
                                       (8  (ecase cc
                                             (1 :luminance8)
                                             (2 :luminance8-alpha8)
                                             (3 :rgb8)
                                             (4 :rgba8)))
                                       (16 (ecase cc
                                             (1 :luminance16)
                                             (2 :luminance16-alpha16)
                                             (3 :rgb16)
                                             (4 :rgba16)))))
                    (border 0)
                    (format (ecase cc
                              (1 :luminance)
                              (2 :luminance-alpha)
                              (3 :rgb)
                              (4 :rgba)))
                    (data-type (ecase (png:image-bit-depth png)
                                 (8  :unsigned-byte)
                                 (16 :unsigned-short))))
		(print internal-format)
		(print format)
		(print data-type)
                (gl:tex-image-2d :texture-2d
                                 level-of-detail
                                 internal-format
                                 ww
                                 hh
                                 border
                                 format
                                 data-type
                                 data))))
	  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
	  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
;;	  (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-S :CLAMP-TO-EDGE)
;;	  (gl:Tex-Parameter :TEXTURE-2D :TEXTURE-WRAP-T :CLAMP-TO-EDGE)
	  (gl:bind-texture :texture-2d 0)
          texture-id)           ; return the texture-id on success

        (error ()
               (unless texture-id-p
                 (gl:delete-textures (list texture-id)))
               nil)
        )))
|#

(defun load-texture (file-name)
  (let* ((image (sdl2-image:load-image file-name))
	 (w (sdl2:surface-width image))
	 (h (sdl2:surface-height image))
	 (bit-depth nil)
	 (internal-format nil)
	 (gl-surface nil)
	 (texture-id 0))
    (setf gl-surface (sdl2:create-rgb-surface-with-format-from (sdl2:surface-pixels image) w h 32 (sdl2:surface-pitch image)))
;;    (sdl2:blit-surface image nil gl-surface nil)
    (setf bit-depth 32
	  internal-format sdl2:+pixelformat-rgba8888+)
    (sdl2:free-surface image)
    (setf texture-id (car (gl:gen-textures 1)))
    (gl:bind-texture :texture-2d texture-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba8 w h 0 :rgba :unsigned-byte (sdl2:surface-pixels gl-surface))
    (sdl2:free-surface gl-surface)
    (gl:bind-texture :texture-2d 0)
    texture-id))

(defvar nika-dress-id nil)

(defstruct tja-bone
  (name "")
  parent
  children ;;list of indices
  (bind-matrix (make-mat4)))
(defstruct tja-frame
  matrices);;makes an array of [num_frames num_bones] long and wide

(defstruct tja-data
  armature-name ;;rig-id
  animation-name
  textures ;;'(skin-num texture-file)
  bones ;;rig-bones
  texture-files ;;files for objs
  weights ;;structure: (aref weights 0) is (bone-weights (aref bones 0)) in engine...I think that having the weights be 'universal' in the animation is a better idea
  ;;i.e. (animation-weights animation) rather than (bone-weights (aref (animation-bones animation) bone-index))
  ;;Yeah, I'm going to do (animation-weights animation)
  frames
  timing)

;;Hey hey people, fuck head here
;;So, v0.1 of the Canis animation subsystem will be completely ram-driven
;;I won't be using shaders, *just* yet
;;I want to make sure that the animation will render correctly first, before implementing it in glsl

(defun parse-tja-matrix (str)
  (let ((mat (make-mat4))
	(cur-ind "")
	(cur-vec ""))
    (dotimes (cur-j 4 t)
      (setf cur-vec (subseq str (1+ (position #\( str)) (position #\) str))
	    str (subseq str (1+ (position #\) str))))
      (dotimes (cur-i 4 t)
	(if (find #\space cur-vec)
	    (setf cur-ind (subseq cur-vec 0 (position #\space cur-vec)))
	    (setf cur-ind cur-vec))
	(if (find #\space cur-vec)
	    (setf cur-vec (subseq cur-vec (1+ (position #\space cur-vec)))))
	(setf (aref mat cur-j cur-i) (parse-float cur-ind))))
    mat))    

(defun parse-armature (ip-stream tja-data)
  (let ((bone-num 0)
	(cur-bone 0)
	(children nil))
    (setf (tja-data-armature-name tja-data) (read-line ip-stream))
    (setf bone-num (parse-integer (read-line ip-stream)))
    (setf (tja-data-bones tja-data) (make-array (list bone-num) :initial-element (make-tja-bone)))
    (loop for cl = (read-line ip-stream)
       while (< cur-bone bone-num)
       do (if (string-equal cl "")
	      (return tja-data))
	 (setf (aref (tja-data-bones tja-data) cur-bone) (make-tja-bone))
	 (let ((cb (aref (tja-data-bones tja-data) cur-bone))
	       (cur-name (subseq cl 0 (position #\[ cl)))
	       (cur-mat (subseq cl (position #\[ cl))))
	   (setf (tja-bone-name cb) (subseq cur-name 0 (1- (length cur-name)))
		 (tja-bone-bind-matrix cb) (parse-tja-matrix cur-mat)))
	 (incf cur-bone))
    (let ((cl (read-line ip-stream)))
      (loop for bone-ind below bone-num
	 do (let ((children-str (subseq cl (+ (position #\: cl) 2) (position #\. cl))))
	      (setf cl (subseq cl (position #\space cl))
		    cl (subseq cl (+ (position #\. cl) 2)))
	      (if (find-if #'digit-char-p children-str)
		  (progn (setf children-str (subseq children-str (position-if #'digit-char-p children-str)))
			 (loop while (> (length children-str) 1)
			    do (if (find-if #'digit-char-p children-str)
				   (push (parse-integer (subseq children-str 0 (position #\space children-str))) children)
				   (return t))
			      (setf children-str (subseq children-str (1+ (position #\space children-str)))))))
	      (setf (tja-bone-children (aref (tja-data-bones tja-data) bone-ind)) children
		    children nil))
	   (if (not (find-if #'digit-char-p cl))
	       (return t))))
    (loop for bone-ind below bone-num
       do (if (tja-bone-children (aref (tja-data-bones tja-data) bone-ind))
	      (loop for child in (tja-bone-children (aref (tja-data-bones tja-data) bone-ind))
		 do (setf (tja-bone-parent (aref (tja-data-bones tja-data) child)) bone-ind))))
    tja-data))

(defmacro parse-tja-weights (str-ip weights cur-w)
  `(loop while (find #\space ,str-ip)
      do (let ((current-array (subseq ,str-ip (1+ (position #\[ ,str-ip)) (position #\] ,str-ip)))
	       (v-ind "")
	       (b-ind "")
	       (w ""))
	   (setf v-ind (subseq current-array 0 (position #\space current-array))
		 current-array (subseq current-array (1+ (position #\space current-array)))
		 b-ind (subseq current-array 0 (position #\space current-array))
		 current-array (subseq current-array (1+ (position #\space current-array)))
		 w current-array)
	   (setf (aref ,weights ,cur-w) (make-array '(3) :initial-contents '(0 0 0.0))
		 (aref (aref ,weights ,cur-w) 0) (parse-integer v-ind)
		 (aref (aref ,weights ,cur-w) 1) (parse-integer b-ind)
		 (aref (aref ,weights ,cur-w) 2) (parse-float w))
	   (setf ,str-ip (subseq ,str-ip (+ (position #\] ,str-ip) 2)))	 
	   (incf ,cur-w))))


(defun parse-animation (ip-stream tja-data)
  (let ((num-bones (car (array-dimensions (tja-data-bones tja-data))))
	(num-frames 0)
	(frames 0)
	(timing nil)
	(timing-string "")
	(frame-data (make-tja-frame))
	(cur-line "")
	)
    (setf (tja-data-animation-name tja-data) (read-line ip-stream)
	  cur-line (read-line ip-stream)
	  num-frames (parse-integer (subseq cur-line (1+ (position #\( cur-line)) (position #\) cur-line)))
	  timing (make-array (list num-frames))
	  timing-string (subseq cur-line (1+ (position #\[ cur-line)) (position #\] cur-line))
	  frames (make-array (list num-frames)))
    (loop for n below num-frames
       do (if (find #\space timing-string)
	      (setf (aref timing n) (parse-float (subseq timing-string 0 (position #\space timing-string)))
		    timing-string (subseq timing-string (1+ (position #\space timing-string))))
	      (setf (aref timing n) (parse-float  timing-string))))
    (setf (tja-data-timing tja-data) timing)
    (let ((frame-time ""))
      (setf cur-line (subseq cur-line (1+ (position #\[ cur-line)) (position #\] cur-line)))
      (loop for frame below num-frames
	 do (if (find #\space cur-line)
		(setf frame-time (subseq cur-line 0 (position #\space cur-line))
		      cur-line (subseq cur-line (1+ (position #\space cur-line))))
		(setf frame-time cur-line
		      cur-line ""))
	   (if (find #\. frame-time)
	       (setf (aref frames frame) (parse-float frame-time))
	       (setf (aref frames frame) (parse-integer frame-time)))))
    (setf (tja-frame-matrices frame-data) (make-array (list num-frames num-bones))) 
    (loop for cur-bone below num-bones
       do (setf cur-line (read-line ip-stream))
	 (loop for frame below num-frames
	    do (let ((cur-line (read-line ip-stream nil)))
		 (if (string-equal cur-line "")
		     (return t))
		 (if (find #\[ cur-line)
		     (setf (aref (tja-frame-matrices frame-data) frame cur-bone)
			   (parse-tja-matrix cur-line))))))
    (setf (tja-data-frames tja-data) (tja-frame-matrices frame-data))
    tja-data))

(defun parse-skins (ip-stream tja-data cl)
  (let ((num-skins 0)
	(num-weights 0)
	(model-name "")
	(texture-name "")
	(skin-name "")
	(cur-skin 0)
	(weights nil)
	(current-weight 0))
    (setf model-name (subseq cl (+ (position #\: cl) 3)))
    (setf cl (read-line ip-stream))
    (setf num-skins (parse-integer (subseq cl 0 (position #\: cl)))
	  num-weights (parse-integer (subseq cl (1+ (position #\: cl))))
	  weights (make-array (list num-weights) :initial-element (make-array '(3) :initial-contents '(0 0 0.0))))
    (loop for cur-line = (read-line ip-stream nil)
       do (if (string-equal cur-line "")
	      (return t))
	 (if (string-equal cur-line "ANIMATIONS")
	     (return (setf tja-data (parse-animation ip-stream tja-data)))
	     (progn (setf skin-name cur-line
			  texture-name (read-line ip-stream))
		    (if (string-equal texture-name "NIL")
			(setf texture-name nil))
		    (if texture-name
			(push (list cur-skin texture-name) (tja-data-textures tja-data)))
		    (let ((current-line (read-line ip-stream)))
		      (parse-tja-weights current-line weights current-weight))
		    (incf cur-skin)
		    (princ "Current Mesh: ") (princ cur-skin) (fresh-line))))
    (setf (tja-data-texture-files tja-data) (reverse (tja-data-texture-files tja-data))
	  (tja-data-weights tja-data) weights)
    tja-data))

(defun tja-importer (file-name)
  (let ((tja-data (make-tja-data)))
    (with-open-file (ip-stream file-name)
      (if (string-equal (read-line ip-stream) "V:1.0")
	  (loop for current-line = (read-line ip-stream nil)
	     do (if (not current-line)
		    (return t))
	       (cond ((string-equal current-line "ARMATURES")
		      (setf tja-data (parse-armature ip-stream tja-data)))
		     ((if (find #\space current-line)
			  (string-equal (subseq current-line 0 (position #\space current-line)) "SKINS"))
		      (setf tja-data (parse-skins ip-stream tja-data current-line)))
		     ((string-equal current-line "ANIMATIONS")
		      (setf tja-data (parse-animation ip-stream tja-data)))))
	  (format t "ERROR: TJA file is not compatible with this parser.")))
    ;;at the end, here,
    tja-data))

(defun t3da-importer (file-name)
  (let ((tja-data (make-tja-data))
	(objs nil)
	(model (make-model))
	(model-info nil)
	(material-library nil)
	(current-object nil)
	(object-name nil)
	(spatial-verts nil)
	(texture-verts nil)
	(normal-verts nil)
	(object-material nil)
	(smoothing nil)
	(polygons nil))
    (with-open-file (ip-stream file-name)
      (if (not (string-equal (read-line ip-stream) "V:1.0"))
	  (format t "ERROR: T3DA file is not compatible with this parser.")
	  (loop for current-line = (read-line ip-stream nil)
	     do (if (not current-line)
		    (return t))
	     ;;	   (princ current-line) ;;debug
	       (if (not (string-equal current-line ""))
		   (cond ((string-equal current-line "ARMATURES")
			  (setf tja-data (parse-armature ip-stream tja-data)))
			 ((if (find #\space current-line)
			      (string-equal (subseq current-line 0 (position #\space current-line)) "SKINS"))
			  (setf tja-data (parse-skins ip-stream tja-data current-line)))
			 ((string-equal current-line "ANIMATIONS")
			  (setf tja-data (parse-animation ip-stream tja-data)))
			 ((char= (aref current-line 0) #\#)
			  (push (subseq current-line 1) model-info))
			 ((char= (aref current-line 0) #\s)
			  (if (char= (aref current-line 2) #\1)
			      (setf smoothing t)
			      (setf smoothing nil)))
			 ((char= (aref current-line 0) #\m)
			  (setf material-library (subseq current-line (position #\space current-line))))
			 ((char= (aref current-line 0) #\v)
			  (cond ((char= (aref current-line 1) #\space)
				 ;;spatial verts
				 (push (parse-obj-vertex current-line) spatial-verts))
				((char= (aref current-line 1) #\t)
				 ;;texture verts
				 (push (parse-obj-uv current-line) texture-verts))
				((char= (aref current-line 1) #\n)
				 ;;normal verts
				 (push (parse-obj-normal current-line) normal-verts))))
			 ((char= (aref current-line 0) #\u)
			  (setf object-material (subseq current-line (position #\space current-line)))
			  (fresh-line))
			 ((char= (aref current-line 0) #\f)
			  (push (parse-obj-face current-line :obj? nil) polygons))
			 ((char= (aref current-line 0) #\o)
			  (if model-info
			      (if (listp model-info)
				  (let ((obj-info ""))
				    (loop for info in (reverse model-info)
				       do (setf obj-info (concatenate 'string obj-info info '(#\newline))))
				    (setf model-info obj-info))))
			  (if current-object
			      (progn (setf current-object (make-object :name object-name
								       :info model-info
								       :material object-material
								       :smoothing smoothing
								       :polygons (make-array (length polygons) :initial-contents (reverse polygons))))
				     (push current-object objs)
				     (setf current-object nil
					   object-material nil
					   object-name nil
					   smoothing nil
					   polygons nil)))
			  (setf current-object (make-object)
				object-name (subseq current-line (position #\space current-line)))))))))
    (setf current-object (make-object :name object-name
				      :info model-info
				      :material object-material
				      :smoothing smoothing
				      :polygons (make-array (length polygons) :initial-contents (reverse polygons))))
    (push current-object objs)
    (let ((vertices (make-array (list (length spatial-verts))))
	  (normal-vertices (make-array (list (length normal-verts))))
	  (texture-vertices (make-array (list (length texture-verts))))
	  (spatial-verts (reverse spatial-verts))
	  (normal-verts (reverse normal-verts))
	  (texture-verts (reverse texture-verts)))
      (loop for index below (length spatial-verts)
	 for spat-vert in spatial-verts
	 do (setf (aref vertices index) (make-vertex))
	   (setf (vertex-values (aref vertices index)) spat-vert))
      (loop for index below (length normal-verts)
	 for norm-vert in normal-verts
	 do (setf (aref normal-vertices index) (make-vertex))
	   (setf (vertex-values (aref normal-vertices index)) norm-vert))
      (loop for index below (length texture-verts)
	 for tex-vert in texture-verts
	 do (setf (aref texture-vertices index) (make-vertex))
	   (setf (vertex-values (aref texture-vertices index)) tex-vert))
      (setf model (make-model :vertices vertices
			      :normal-vertices normal-vertices
			      :texture-vertices texture-vertices
			      :objects (reverse objs)
			      :animation tja-data))
      (setf (model-scale-scalar model) 12.0)
      model)))
    


;;(setf (object-texture-id (cadr (model-objects nika))) (load-png #P"/home/neon-frost/Work Files/3D/Nika Dress Tex - Dress Patterns.png"))

#|

(defun save-model (model file-name)
  )

(defmacro load-model (model file-name)
  )

|#
