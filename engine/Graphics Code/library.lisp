#|
==============================================================================
                                GFX
==============================================================================
|#
(defmacro define-buffer (buffer)
  `(progn (defvar ,buffer nil)
	  (push ,buffer buffers)))

(defstruct sprite-sheet
  width
  height
  cells
  file
  surface
  texture)

(defun create-texture (&key (format sdl2:+pixelformat-rgba8888+) (access 0) (width 16) (height 16) (color +white+))
  (sdl2:set-render-draw-color renderer (car color) (cadr color) (caddr color) (cadddr color))
  (sdl2:render-clear renderer)
  (sdl2:create-texture renderer format access width height))

(defmacro with-rectangle (name rect &body body)
  `(let ((,name (sdl2:make-rect (car ,rect)
				(cadr ,rect)
				(caddr ,rect)
				(cadddr ,rect))))
     ,@body
     (sdl2:free-rect ,name)))
(defmacro create-rectangle (rect-vals)
  `(sdl2:make-rect (car ,rect-vals)
		   (cadr ,rect-vals)
		   (caddr ,rect-vals)
		   (cadddr ,rect-vals)))	   

(defmacro optimize-sheet (var)
  `(setf (sprite-sheet-texture ,var) (sdl2:create-texture-from-surface renderer (sprite-sheet-surface ,var)))
  )

(defmacro set-sheet-width (sheet width)
  `(setf (sprite-sheet-width ,sheet) ,width)
  )

(defmacro set-sheet-height (sheet height)
  `(setf (sprite-sheet-height ,sheet) ,height)
  )

(defmacro set-cells (sheet tile-size)
  `(let ((cells (loop for y below (sprite-sheet-height ,sheet) by (cadr ,tile-size)
		   append (loop for x below (sprite-sheet-width ,sheet) by (car ,tile-size)
			     collect (list x y (car ,tile-size) (cadr ,tile-size))))
	   ))
     (setf (sprite-sheet-cells ,sheet) cells)
     ))

(defmacro set-sheet-surface (sheet surface)
  `(setf (sprite-sheet-surface ,sheet) ,surface))

(defmacro load-sheet (sheet cell-size)
  `(let* ((filename (sprite-sheet-file ,sheet))
	  (surface (sdl2-image:load-image filename))
	  )
     (set-sheet-height ,sheet (sdl2:surface-height surface))
     (set-sheet-width ,sheet (sdl2:surface-width surface))
     (set-cells ,sheet ,cell-size)
     (set-sheet-surface ,sheet surface)
     (optimize-sheet ,sheet)
     ))

(defmacro defsheet (entity file cell-size)
  `(progn (setf (entity-sheet-surface ,entity) (make-sprite-sheet :file ,file))
	  (load-sheet (entity-sheet-surface ,entity) ,cell-size)
	  ))

(defmacro tex-blit (tex &key (src nil) dest color angle center (flip :none))
  `(progn (if ,color
	      (sdl2:set-texture-color-mod ,tex (car ,color) (cadr ,color) (caddr ,color)))
	  (if (not ,src)
	      (let ((src (sdl2:make-rect 0 0 (sdl2:texture-width ,tex) (sdl2:texture-height ,tex))))
		(sdl2:render-copy-ex renderer
				     ,tex
				     :source-rect src
				     :dest-rect ,dest
				     :angle ,angle
				     :center ,center
				     :flip (list ,flip))
		(sdl2:free-rect src)
		(sdl2:free-rect ,dest))
	      (progn (sdl2:render-copy-ex renderer
					  ,tex
					  :source-rect ,src
					  :dest-rect ,dest
					  :angle ,angle
					  :center ,center
					  :flip (list ,flip))
		     (sdl2:free-rect ,src)
		     (sdl2:free-rect ,dest))
	      )))

(defmacro draw-cell (sheet cell x y &key width height color (angle 0) center (flip :none))
  `(let* ((cells (sprite-sheet-cells ,sheet))
	  (src-rect (sdl2:make-rect (nth 0 (nth ,cell cells))
				    (nth 1 (nth ,cell cells))
				    (nth 2 (nth ,cell cells))
				    (nth 3 (nth ,cell cells))))
	  (tsx (nth 2 (nth ,cell cells)))
	  (tsy (nth 3 (nth ,cell cells)))
	  (dest-rect (sdl2:make-rect ,x
				     ,y
				     (if ,width
					 ,width
					 tsx)
				     (if ,height
					 ,height
					 tsy)))
	  (flip (if (or (eq ,flip :none)
			(eq ,flip :horizontal)
			(eq ,flip :vertical))
		    ,flip
		    :none))
	  (center (if ,center
		      ,center
		      nil))
	  (angle (if (not (or (integerp ,angle)
			      (floatp ,angle)))
		     0
		     ,angle)))
     (tex-blit (sprite-sheet-texture ,sheet) :src src-rect :dest dest-rect :color ,color :angle angle :center center :flip flip)
     ))

(defmacro free-sheet (sheet)
  `(progn (sdl2:destroy-texture (sprite-sheet-texture ,sheet))
	  (setf ,sheet nil)))

(defun render-box (x y w h &key color)
  (let* ((color (if (not color)
		    '(255 255 255 255)
		    color))
	 (r (car color))
	 (g (cadr color))
	 (b (caddr color))
	 (a (or (cadddr color)
		255))
	 (rect (sdl2:make-rect x y w h)))
    (sdl2:set-render-draw-color renderer r g b a)
    (sdl2:render-fill-rect renderer rect)
    (sdl2:free-rect rect)))

(defmacro reset-text-buffer (buffer)
  `(if ,buffer
       (progn (sdl2:destroy-texture ,buffer)
	      (setf ,buffer nil))))

(defun render-buffer (buffer menu &key color)
  (if color
      (sdl2:set-texture-color-mod buffer (car color) (cadr color) (caddr color)))
  (let ((src (sdl2:make-rect 0
			     0
			     (sdl2:texture-width buffer)
			     (sdl2:texture-height buffer)
			     ))
	(dest (sdl2:make-rect (+ (menu-x menu) 8)
			      (+ (menu-y menu) 8)
			      (- (menu-width menu) 8)
			      (- (menu-height menu) 8)
			      )))
    (sdl2:render-copy renderer
		      buffer
		      :source-rect src				    
		      :dest-rect dest)
    (sdl2:free-rect src)
    (sdl2:free-rect dest)))

(defvar current-font-color '(127 0 0 255))
(defun render-string (str x y &key width height (color current-font-color))
  (let ((w (or width
	       (if (find #\newline str)
		   (* (position #\newline str) (car character-size)))
	       (* (length str) (car character-size))))
	(h (or height
	       (* (1+ (count #\newline str)) (cadr character-size))))
	(width (or width
		   0))
	(height (or height
		    0)))
#|    (if (find #\newline str)
	(if (> (* (position #\newline str) (car character-size)) w)
	    (setf w (* (position #\newline str) (car character-size)))))|#
    (if (> (length str) 0)
	(let* ((string-buffer (create-text-buffer str
						  :width w
						  :height h
						  :to-texture t
						  :string-case 'text))
	       (w (if (> width (sdl2:texture-width string-buffer))
		      width
		      (sdl2:texture-width string-buffer)))
	       (h (if (> height (sdl2:texture-height string-buffer))
		      height
		      (sdl2:texture-height string-buffer))))
	  (tex-blit string-buffer :src (create-rectangle (list 0 0 w h))
		    :dest (create-rectangle (list x y w h))
		    :color color)
	  (reset-text-buffer string-buffer)))))
#|
==============================================================================
                                 BATTLE
==============================================================================
|#

(defun draw-line (x y x2 y2 &key (color '(255 255 255 255)))
  (let ((r (car color))
	(g (cadr color))
	(b (caddr color))
	(a (cadddr color))
	)
    (sdl2:set-render-draw-color renderer r g b a)
    (sdl2:render-draw-line renderer x y x2 y2)))

(defun draw-box (x y w h color)
  (let ((rect (sdl2:make-rect x y w h))
	(r (car color))
	(g (cadr color))
	(b (caddr color))
	(a 255)
	)
    (sdl2:set-render-draw-color screen-surface r g b a)
    (sdl2:render-fill-rect renderer rect)
    (sdl2:free-rect rect)))
