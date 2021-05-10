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
(load "lib/math-lib.cl")
(load "lib/lib.cl")
(load "lib/vao.cl")
(load "assets/plane.cl")
(load "lib/drawing-lib.cl")

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

(defvar move-socra? nil)

(defvar object-matrix (make-mat4))
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

(setf *vert-shader* "#version 140

// Input vertex data, different for all executions of this shader.
//layout(location = 0) in vec3 vertexPosition_modelspace;
in vec3 vertexPosition_modelspace;
//layout(location = 1) in vec2 vertexUV;
//in vec3 vertexPosition_modelspace;
//out vec4 colour;

// Output data ; will be interpolated for each fragment.
//out vec2 UV;
//const vec4 white = vec4(vertexPosition_modelspace, 1.0);

// Values that stay constant for the whole mesh.
//uniform mat4 MVP;

void main(){
//colour = vec4(1.0);
    // Output position of the vertex, in clip space : MVP * position
//    gl_Position =  MVP * vec4(vertexPosition_modelspace,1);
gl_Position = vec4(vertexPosition_modelspace, 1.0);

    // UV of the vertex. No special space for this one.
//    UV = vertexUV;

}")
(setf *vert-shader* "#version 140

// Input vertex data, different for all executions of this shader.
in vec3 vertexPosition_modelspace;

void main(){
gl_Position = vec4(vertexPosition_modelspace, 1.0);

}")

(defvar *frag-shader*
  "#version 130
//precision highp float;
//in vec4 colour;

out vec4 fragcolor;

void main()
{
	fragcolor = vec4(0.0,1.0,0.0,1.0);
}")

(defvar shader-prog nil)

(load "importers/importer.cl")

(defvar polygon-count 0)

(load "matrices-for-3d.cl")


;;(setf (model-key nika) 0)

(defvar nika-timer 0)

(defvar nika-bent-jump nil)

(defvar bones-printed? nil)

;;(incf (model-z nika-roller) -3)

(load "3d-drawing-function.cl")



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
  (let ((asp (/ 300 400.0))) ;;asp = width/height
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
  (alt-draw plane;; socra test-tower dress-plane
;;	    nika
	    ;;	    alt-nika
;;	    robert
;;	    nika-bent-jump
;;	    nika-roller
     )
  |#
  (draw-vaos plane-2)
  )

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
	(sdl2:set-window-size win 1000 700)
	(gl:viewport 0 0 1000 700)
;	(gl:shade-model :smooth)
	(gl:matrix-mode :projection)
	(gl:load-identity)
	(gl:ortho -100 100 -100 100 -200 200)
	;;	(gl:cull-face :back)
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
	(gl:clear-color 0.2 0.2 0.2 1.0)
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

	  (gl:bind-attrib-location shader-prog 0 "vertexPosition_modelspace")
	  
	  ;; Don't forget to link the program after attaching the
	  ;; shaders. This step actually puts the attached shader together
	  ;; to form the program.
	  (gl:link-program shader-prog)
	  ;; If we want to render using this program object, or add
	  ;; uniforms, we need to use the program. This is similar to
	  ;; binding a buffer.
	  (gl:use-program shader-prog)
	  (create-spatial-vao plane-2)
;;	  (sdl2:hide-cursor)
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
		   (gl:get-error)
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


(defun print-messages ()
  (loop for msg in (reverse tamias-3d-messages)
     do (princ msg)
       (fresh-line)))
(print-messages)

