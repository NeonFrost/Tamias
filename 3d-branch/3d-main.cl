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
(defvar *frag-shader*
  "#version 130
void main()
{
	gl_FragColor = gl_Color; //vec4(0.4,0.4,0.8,1.0);
}
")

(load "importer.cl")

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

