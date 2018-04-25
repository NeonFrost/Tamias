#|
==============================================================================
                                GFX
==============================================================================
|#
(defmacro define-buffer (buffer)
  `(progn (defvar ,buffer nil)
	  (push ,buffer buffers)))
(defvar tile-buffer nil)

(defstruct sprite-sheet
  width
  height
  cells
  file
  surface
  texture)

;;Now for some 'helper' functions

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
		    '(0 0 0 255)
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

(defun create-tile-buffer (surface sheet tile x y)
  (let* ((cells  (sprite-sheet-cells sheet))
	 (src-rect (sdl2:make-rect (nth 0 (nth tile cells))
				   (nth 1 (nth tile cells))
				   (nth 2 (nth tile cells))
				   (nth 3 (nth tile cells))))
	 (tsx (nth 2 (nth tile cells)))
	 (tsy (nth 3 (nth tile cells)))
	 (dest-rect (sdl2:make-rect x
				    y
				    tsx
				    tsy)))
    (blit (sprite-sheet-surface sheet) src-rect surface dest-rect)
    (sdl2:free-rect src-rect)
    (sdl2:free-rect dest-rect)))

(defmacro reset-text-buffer (buffer)
  `(if ,buffer
       (progn (sdl2:destroy-texture ,buffer)
	      (setf ,buffer nil))))

(defun render-buffer (buffer menu &key color)
  (sdl2:set-texture-color-mod buffer (car color) (cadr color) (caddr color))
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

(defun render-string (x y w h &rest strs)
  (let ((str ""))
    (loop for str-t in strs
       do (setf str (combine-strings str (if (not (stringp str-t))
					     (write-to-string str-t)
					     str-t))))
    (let ((string-buffer (create-text-buffer str :width (if (find #\newline str)
							    (if (> (* (position #\newline str) (car character-size)) w)
								w
								(* (position #\newline str) (car character-size)))
							    (if (> (* (length str) (car character-size)) w)
								w
								(* (length str) (car character-size))))
					     :height (* (1+ (count #\newline str)) (cadr character-size))
					     :to-texture t
					     :string-case 'text)))
      (tex-blit string-buffer :dest (create-rectangle (list x y w h)))
      (reset-text-buffer string-buffer))))
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

#|
(defmacro draw-rectangle (x y w h color)
  "Draws the 'outline' of a rectangle"
  `(let ((rect (sdl2:make-rect ,x ,y ,w ,h))
	 (r (car color))
	 (g (cadr color))
	 (b (caddr color))
	 (a 255)
	 )
     (sdl2:set-render-draw-color screen-surface r g b a)
     (sdl2:render-draw-rect screen-surface rect)
     )
  )
(defmacro draw-battle-menu (x y w h color)
  `(progn (draw-box ,x ,y ,w ,h ,color)
	  (draw- ,x ,y ,w ,h *white*))
  )

(defmacro draw-battle-string (str x y)
;;;;  `(sdl:draw-string-at-* ,str ,x ,y))
`(let* ((surface (sdl2-ttf:render-text-solid *font* ,str (car *font-color*) (cadr *font-color*) (caddr *font-color*) 0))
(texture (sdl2:create-texture-from-surface surface)))
(free-surface surface)
(render-copy renderer
	     texture
	     :source-rect (cffi:null-pointer)
	     :dest-rect (make-rect ,x ,y
				   (texture-width texture)
				   (texture-height texture)))
(destroy-texture texture)
))

(defmacro draw-icon (icon-cell x y) ;TODO: update
`(sdl:draw-surface-at-* *icon-sheet* ,x ,y :cell ,icon-cell))
(defmacro rend-monster (x y monster)
  `(if (> (monster-hp ,monster) 0)
       (sdl:draw-surface-at-* *monsters* (- (round ,x) 64) ,y :cell (monster-cell ,monster))
       ))

#|
==============================================================================
                                  DIALOG
==============================================================================
|#

(defmacro draw-d-box (db)
  `(sdl:draw-surface-at-* ,db 0 (- screen-height 32))
  )
(defmacro draw-d-string (str y)
    `(let* ((surface (sdl2-ttf:render-text-solid *font* ,str (car *font-color*) (cadr *font-color*) (caddr *font-color*) 0))
	    (texture (sdl2:create-texture-from-surface surface)))
       (free-surface surface)
       (render-copy renderer
		    texture
		    :source-rect (cffi:null-pointer)
		    :dest-rect (make-rect 64 ,y
					  (texture-width texture)
					  (texture-height texture)))
       (destroy-texture texture)
       ))|#

