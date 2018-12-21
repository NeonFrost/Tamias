#|
Uses vectors to move from one location to the next

|#
(defstruct particle
  vector
  size
  duration
  (lifetime 0)
  color)

#|
On 'z'
it creates a new particle
this particle has no name, it is part of a particles list
|#

(defmacro draw-particle (rect color)
  `(let ((r (car ,color))
	 (g (cadr ,color))
	 (b (caddr ,color))
	 (a (cadddr ,color)))
     (sdl2:set-render-draw-color renderer r g b a)
     (sdl2:render-fill-rect renderer ,rect)))

(defmacro create-particle (vector size duration color particles)
  `(push-particle (make-particle :vector ,vector :size ,size :duration ,duration :color ,color) ,particles))
(defmacro decay-particle (particle)
  `(progn (incf (particle-lifetime ,particle) 1)
	  (incf (car (particle-size ,particle)) (vector-3d-x (particle-vector ,particle)))
	  (incf (cadr (particle-size ,particle)) (vector-3d-y (particle-vector ,particle)))))
(defmacro kill-particle (particle)
  `(setf ,particle nil))
(defmacro push-particle (particle particles)
  `(if ,particles
       (setf ,particles (append ,particles (list ,particle)))
       (setf ,particles (list ,particle))))
(defun check-particles (particles) ;;list of particles
  (loop for particle in particles
     do (if (>= (particle-lifetime particle) (particle-duration particle))
	    (setf (nth (position particle particles) particles) nil)))
  (setf particles (remove nil particles)))

(defun particle-effect (particle)
  (if particle
      (let* ((color (particle-color particle))
	     (size (particle-size particle))
	     (duration (particle-duration particle))
	     (lifetime (particle-lifetime particle))
	     (rect (sdl2:make-rect (car size)
				   (cadr size)
				   (caddr size)
				   (cadddr size)))
	     (r (car color))
	     (g (cadr color))
	     (b (caddr color))
	     (a (cadddr color)))
	(if (> duration lifetime)
	    (draw-particle rect (list r g b a)))
	(sdl2:free-rect rect)
	(decay-particle particle)
	)))
