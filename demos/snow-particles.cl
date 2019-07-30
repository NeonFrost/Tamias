(define-state snow-particles)
(setf state snow-particles)
(setf resolution 10)

(defstruct (nika (:include entity)))
(defvar nika (make-nika :x 480 :y 480 :width 128 :height 128))
(define-track snowy-fields-track "demos/Snowy fields of Karakas.ogg")
;(start-track snowy-fields-track)

(defvar nika-sheet (make-sprite-sheet :file "demos/Nika.png"))
(defvar background nil)

(defvar nika-current-cell 0)
(defvar dir 1)
(defvar cell-timer 0)
(defvar moving nil)
(defvar 2nd-moving nil)
(defvar particles-of-snow nil)
(defvar footprints nil)
(setf tamias-renderer-clear-color '(100 100 100 255))
(setf (nika-width nika) 128
      (nika-height nika) 128)

(defun draw-nika-at (x y)
  (draw-cell nika-sheet nika-current-cell
	     x y
	     :width (nika-width nika) :height (nika-height nika)
	     :color '(200 200 235 255)))

(defun draw-nika ()
  (if background
      (tex-blit background
		:src (sdl2:make-rect 0 0
				     (sdl2:texture-width background)
				     (sdl2:texture-height background))
		:dest (sdl2:make-rect 0 0
				      1920
				      1080)
		:color '(220 220 255 255))
      (setf background (sdl2:create-texture-from-surface renderer (sdl2-image:load-image "demos/snow-test.png"))))
  (if footprints
      (loop for footprint in footprints
	 do (render-box (car footprint) (cadr footprint)
			4 4
			:color +dark-pastel-grey+)))
  (if (not (sprite-sheet-cells nika-sheet))
      (load-sheet nika-sheet '(64 64)))
  (if (> (nika-x nika) (- *screen-width* (nika-width nika)))
      (draw-nika-at (-  (nika-x nika) *screen-width*) (nika-y nika))
      (if (< (nika-x nika) 0)
	  (draw-nika-at (+ *screen-width* (nika-x nika)) (nika-y nika))))
  (if (> (nika-y nika) (- *screen-height* (nika-height nika)))
      (draw-nika-at (nika-x nika) (- (nika-y nika) *screen-height*))
      (if (< (nika-y nika) 0)
	  (draw-nika-at (nika-x nika) (+ *screen-height* (nika-y nika)))))
  (if (and (> (nika-x nika) (- *screen-width* (nika-width nika)))
	   (> (nika-y nika) (- *screen-height* (nika-width nika))))
      (draw-nika-at (- (nika-x nika) *screen-width*) (- (nika-y nika) *screen-height*))
      (if (and (< (nika-x nika) 0)
	       (< (nika-y nika) 0))
	  (draw-nika-at (+ *screen-width* (nika-x nika)) (+ *screen-height* (nika-y nika)))))
  (draw-nika-at (nika-x nika) (nika-y nika)))
(add-to-state-render draw-nika snow-particles)

(defun draw-snow-particles ()
  (if (< (length particles-of-snow) 10000000)
      (loop for fuck below 5
	   do (create-particle (make-vector-3d :x -4 :y (1+ (random 3)))
		       (list (+ *screen-width* (random 160)) (- (random (+ 384 *screen-height*)) 384) (+ 4 (random 6)) (+ 4 (random 6)))
		       (* fps 20)
		       +white+
		       particles-of-snow)))
  (if particles-of-snow
      (loop for particle in particles-of-snow
	 do (if particle
		(progn (particle-effect particle)
		       (decay-particle particle)
		       (if (>= (particle-lifetime particle) (particle-duration particle))
			   (setf (nth (position particle particles-of-snow) particles-of-snow) nil))))))
  (setf particles-of-snow (remove nil particles-of-snow)))
(add-to-state-render draw-snow-particles snow-particles)

(defun increase-cell ()
  (if (not (sprite-sheet-cells nika-sheet))
      (load-sheet nika-sheet '(64 64)))
  (if moving
      (incf cell-timer 5)
      (setf nika-current-cell (- (* dir 8) 8)))
  (if (> cell-timer 5)
      (progn (incf nika-current-cell 1)
	     (setf cell-timer 0)))
  (if (>= nika-current-cell (* dir 8))
      (setf nika-current-cell (- (* dir 8) 8)))
  (if (< nika-current-cell (- (* dir 8) 8))
      (setf nika-current-cell (- (* dir 8) 8))))
(add-loop-function increase-cell snow-particles 'top)

(defun move-nika ()
  (if (and (eq (vector-3d-x (nika-vector nika)) 0)
	   (eq (vector-3d-y (nika-vector nika)) 0))
      (setf moving nil))
  (incf (nika-x nika) (vector-3d-x (nika-vector nika)))
  (incf (nika-y nika) (vector-3d-y (nika-vector nika)))
  (if (> (nika-x nika) *screen-width*)
      (setf (nika-x nika) (vector-3d-x (nika-vector nika)))
      (if (< (nika-x nika) (- 0 (nika-width nika)))
	  (progn (setf (nika-x nika) (- *screen-width* (nika-width nika)))
		 (incf (nika-x nika) (vector-3d-x (nika-vector nika))))))
  (if (> (nika-y nika) *screen-height*)
      (setf (nika-y nika) (vector-3d-y (nika-vector nika)))
      (if (< (nika-y nika) (- 0 (nika-height nika)))
	  (progn (setf (nika-y nika) (- *screen-height* (nika-height nika)))
		 (incf (nika-y nika) (vector-3d-y (nika-vector nika))))))
  (if (not (eq (vector-3d-y (nika-vector nika)) 0))
      (if (> (vector-3d-y (nika-vector nika)) 0)
	  (setf dir 1)
	  (setf dir 2))
      (if (not (eq (vector-3d-x (nika-vector nika)) 0))
	  (if (> (vector-3d-x (nika-vector nika)) 0)
	      (setf dir 3)
	      (setf dir 4))))
  (if moving
      (if (< (length footprints) 50)
	  (push (list (round (- (+ (nika-x nika) (nika-width nika)) (/ (nika-width nika) 2)))
		      (- (+ (nika-y nika) (nika-height nika)) 8)) footprints)
	  (progn (setf (nth 49 footprints) nil)
		 (setf footprints (remove nil footprints)))))
  )
(add-loop-function move-nika snow-particles 'top)

(defmacro reset-nika (dir)
  `(if (not moving)
       (setf dir ,dir
	     nika-current-cell (- (* dir 8) 8))))

(defmacro set-nika (dir)
  `(progn (if (not moving)
	      (setf dir ,dir
		    nika-current-cell (- (* dir 8) 8)))
	  (if (not (eq (vector-3d-y (nika-vector nika)) 0))
	      (if (> (vector-3d-y (nika-vector nika)) 0)
		  (setf dir 1)
		  (setf dir 2))
	      (if (not (eq (vector-3d-x (nika-vector nika)) 0))
		  (if (> (vector-3d-x (nika-vector nika)) 0)
		      (setf dir 3)
		      (setf dir 4))))
	  (setf moving t)
	  (case ,dir
	    (1 (setf (vector-3d-y (nika-vector nika)) 8))
	    (2 (setf (vector-3d-y (nika-vector nika)) -8))
	    (3 (setf (vector-3d-x (nika-vector nika)) 8))
	    (4 (setf (vector-3d-x (nika-vector nika)) -8)))))
	    
(add-key :down snow-particles :down (set-nika 1))
(add-key :up snow-particles :down (set-nika 2))
(add-key :right snow-particles :down (set-nika 3))
(add-key :left snow-particles :down (set-nika 4))
(add-key :down snow-particles :up (reset-nika 1)
	 (setf (vector-3d-y (nika-vector nika)) 0))
(add-key :up snow-particles :up (reset-nika 2)
	 (setf (vector-3d-y (nika-vector nika)) 0))
(add-key :right snow-particles :up (reset-nika 3)
	 (setf (vector-3d-x (nika-vector nika)) 0))
(add-key :left snow-particles :up (reset-nika 4)
	 (setf (vector-3d-x (nika-vector nika)) 0))

