#|
==============================================================================
                                GENERAL
==============================================================================
|#
(defun draw-line (x y x2 y2 &key (color '(255 255 255 255)))
  (let ((r (car color))
	(g (cadr color))
	(b (caddr color))
	(a (cadddr color)))
    (sdl2:set-render-draw-color renderer r g b a)
    (sdl2:render-draw-line renderer x y x2 y2)))

(defmacro define-buffer (buffer)
  `(progn (defvar ,buffer nil)
	  (push ,buffer buffers)))

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

(defmacro tex-blit (tex &key src dest color angle center (flip :none))
  `(progn (if ,color
	      (progn (sdl2:set-texture-color-mod ,tex (car ,color) (cadr ,color) (caddr ,color))
		     (if (> (length ,color) 3)
			 (sdl2:set-texture-alpha-mod ,tex (nth 3 ,color)))))
	  
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

(defun render-box (x y w h &key (color '(255 255 255 255)) (filled t))
  (let* ((r (car color))
	 (g (cadr color))
	 (b (caddr color))
	 (a (or (cadddr color)
		255))
	 (rect (sdl2:make-rect x y w h))) ;; I still want to figure out a way to better manage memory with Tamias, currently, a rectangle is made and then freed every frame
    ;;set the render draw color to the color
    (sdl2:set-render-draw-color renderer r g b a)
    (if filled
	(sdl2:render-fill-rect renderer rect) ;;fills the area of x -> w and y -> h with color (default white)
	(sdl2:render-draw-rect renderer rect)) ;;draws 4 lines, x -> , x -> w, y -> w, h -> w with color (default white)
    (sdl2:free-rect rect))) ;;frees memory taken up by the rect. Not doing this will cause a segfault

(defun render-rectangle (x y w h &key (color '(255 255 255 255)) filled)
  (render-box x y w h :color color :filled filled))

(defun draw-box (x y w h &key (color '(255 255 255 255)) (filled t))
  (render-box x y w h :color color :filled filled))
(defun draw-rectangle (x y w h &key (color '(255 255 255 255)) filled)
  (render-box x y w h :color color :filled filled))
#|  (let ((rect (sdl2:make-rect x y w h))
	(r (car color))
	(g (cadr color))
	(b (caddr color))
	(a 255)
	)
    (sdl2:set-render-draw-color screen-surface r g b a)
    (sdl2:render-fill-rect renderer rect)
    (sdl2:free-rect rect)))|#


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

(defun text-width (text)
  (* (length text) (car character-size))
  )
(defun text-height (text)
  (let ((new-line-count (1+ (count #\newline text))))
    (* new-line-count (cadr character-size))))
(defun text-dimensions (text)
  (if (symbolp text)
      (setf text (write-to-string text)))
  (if (stringp text)
      (let ((width (text-width text))
	    (height (text-height text)))
	(list width height))
      (list 0 0)))


(defun render-string (str x y &key width height dest-width dest-height (rotate 0) (color current-font-color) anti-alias)
  (let ((w (or width
	       (if (find #\newline str)
		   (* (position #\newline str) (car character-size)))
	       (* (length str) (car character-size))))
	(h (or height
	       (* (1+ (count #\newline str)) (cadr character-size))))
	(width (or width
		   dest-width
		   0))
	(height (or height
		    dest-height
		    0)))
#|    (if (find #\newline str)
	(if (> (* (position #\newline str) (car character-size)) w)
	    (setf w (* (position #\newline str) (car character-size)))))|#
    (if (> (length str) 0)
	(let* ((string-buffer (create-text-buffer str
						  :width w
						  :height h))
	       (w (if (> width (sdl2:texture-width string-buffer))
		      width
		      (sdl2:texture-width string-buffer)))
	       (h (if (> height (sdl2:texture-height string-buffer))
		      height
		      (sdl2:texture-height string-buffer)))
	       (dw (or dest-width
			  w))
	       (dh (or dest-height
			   h)))
	  (tex-blit string-buffer :src (create-rectangle (list 0 0 w h))
		    :dest (create-rectangle (list x y dw dh))
		    :color color
		    :angle rotate)
	  (if anti-alias
	      (let ((alpha-color (list (car color) (cadr color) (caddr color) 125)))
		(tex-blit string-buffer :src (create-rectangle (list 0 0 w h))
			  :dest (create-rectangle (list (1- x) y dw dh))
			  :color alpha-color
			  :angle rotate)
		(tex-blit string-buffer :src (create-rectangle (list 0 0 w h))
			  :dest (create-rectangle (list (1+ x) y dw dh))
			  :color alpha-color
			  :angle rotate)
		(tex-blit string-buffer :src (create-rectangle (list 0 0 w h))
			  :dest (create-rectangle (list x (1- y) dw dh))
			  :color alpha-color
			  :angle rotate)
		(tex-blit string-buffer :src (create-rectangle (list 0 0 w h))
			  :dest (create-rectangle (list x (1+ y) dw dh))
			  :color alpha-color
			  :angle rotate)))
	  (reset-text-buffer string-buffer)))))

(defun render-text (str x y &key width height dest-width dest-height (rotate 0) (color current-font-color) scale)
  (if scale
      (let ((w (or width
		   (if (find #\newline str)
		       (* (position #\newline str) (car character-size)))
		   (* (length str) (car character-size))))
	    (h (or height
		   (* (1+ (count #\newline str)) (cadr character-size))))
	    (dest-width (or width
		       dest-width
		       0))
	    (dest-height (or height
			dest-height
			0)))
	;; stretches the text to the scale
	(setf dest-width (round (* w scale))
	      dest-height (round (* h scale))
	      x (- x (round (/ dest-width 2)))
	      y (- y (round (/ dest-height 2))))
	(render-string str x y :width width :height height :dest-width dest-width :dest-height dest-height :rotate rotate :color color :anti-alias t))
      (render-string str x y :width width :height height :dest-width dest-width :dest-height dest-height :rotate rotate :color color :anti-alias t)))

#|
old text-buffer code
       do (setf (values cell-row cell-column) (truncate (char-code (aref string n)) 16))
       ;;In order to display the text-strings correctly, the code must check for a newline character before n and use that for the divisor,
       ;;or use the position of #\Newline if there is no newline character before n
	 (if (find #\NewLine string)
	     (case buffer-source
	       (array (setf (values mod-y mod-x) (truncate n (1+ (position #\Newline string)))))
	       (text (incf mod-x 1)
		     (incf temp-value 1)
		     (if (eq (aref string n) #\NewLine)
			 (progn (setf mod-x -1)
				(incf mod-y 1)
				(setf temp-value 0))
			 (if (and (eq (mod temp-value (/ width (car character-size))) 0)
				  (> mod-x 0))
			     (progn (setf mod-x 0)
				    (incf mod-y 1)
				    (setf temp-value 0))))))
	     (if (eq buffer-source 'text)
		 (progn (incf mod-x 1)
			(if (and (eq (mod n (/ width (car character-size))) 0)
				 (> mod-x 0))
			    (progn (setf mod-x 0)
				   (incf mod-y 1))))
		 (setf (values mod-y mod-x) (truncate n (length string)))))
	 (if (not (eq (char string n) #\NewLine))
	     (let ((color (case (aref string n)
			    (#\w (list 10 10 200))
			    (#\O (list 66 34 10))
			    (#\# (list 126 94 60))
			    (#\. (list 16 60 17))
			    (otherwise (list 255 255 255)))))
	       (if (eq buffer-source 'array)
		   (if (not (eq (char string n) #\0))
	 (render-character-to-buffer (list cell-row cell-column)
						   (+ x (* mod-x (car character-size)))
						   (+ y (* mod-y (cadr character-size)))
						   buff
						   :color color))
		   (render-character-to-buffer (list cell-row cell-column)
					       (+ x (* mod-x (car character-size)))
					       (+ y (* mod-y (cadr character-size)))
					       buff
					       :color (list 255 255 255)))
	       )))
|#


#|
==============================================================================
                            SPRITES and ANIMATION
==============================================================================
|#

(defstruct sprite-sheet
  file
  width
  height
  cells
  rows
  columns
  surface
  texture
  (cell-timer 0)
  (cell-max-time 0)
  (current-cell 0))


(defmacro optimize-sheet (var)
  `(setf (sprite-sheet-texture ,var) (sdl2:create-texture-from-surface renderer (sprite-sheet-surface ,var))))

(defmacro set-sheet-width (sheet width)
  `(setf (sprite-sheet-width ,sheet) ,width))

(defmacro set-sheet-height (sheet height)
  `(setf (sprite-sheet-height ,sheet) ,height))

(defmacro set-cells (sheet tile-size)
  `(let ((rows (/ (sprite-sheet-height ,sheet) (cadr ,tile-size)))
	 (columns (/ (sprite-sheet-width ,sheet) (car ,tile-size)))
	 (cells (loop for y below (sprite-sheet-height ,sheet) by (cadr ,tile-size)
		   append (loop for x below (sprite-sheet-width ,sheet) by (car ,tile-size)
			     collect (list x y (car ,tile-size) (cadr ,tile-size))))))
     (setf (sprite-sheet-rows ,sheet) rows
	   (sprite-sheet-columns ,sheet) columns
	   (sprite-sheet-cells ,sheet) cells)
     ))

(defmacro set-sheet-surface (sheet surface)
  `(setf (sprite-sheet-surface ,sheet) ,surface))

(defmacro load-sheet (sheet cell-size)
  `(let* ((filename (sprite-sheet-file ,sheet))
	  (surface (sdl2-image:load-image filename)))
     (set-sheet-height ,sheet (sdl2:surface-height surface))
     (set-sheet-width ,sheet (sdl2:surface-width surface))
     (set-cells ,sheet ,cell-size)
     (set-sheet-surface ,sheet surface)
     (optimize-sheet ,sheet)))

(defmacro defsheet (entity file cell-size)
  `(progn (setf (entity-sheet-surface ,entity) (make-sprite-sheet :file ,file))
	  (load-sheet (entity-sheet-surface ,entity) ,cell-size)))

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
     (tex-blit (sprite-sheet-texture ,sheet)
	       :src src-rect :dest dest-rect
	       :color ,color :angle angle :center center :flip flip)))
(defmacro draw-tile (sheet cell x y &key width height color (angle 0) center (flip :none))
  `(draw-cell ,sheet ,cell
	     ,x ,y
	     :width ,width :height ,height :color ,color :angle ,angle :center ,center :flip ,flip))

(defmacro free-sheet (sheet)
  `(progn (sdl2:destroy-texture (sprite-sheet-texture ,sheet))
	  (setf ,sheet nil)))

(defun tamias-animate (sheet x y &key width height (current-row 1) color (angle 0) center (flip :none))
  (incf (sprite-sheet-cell-timer sheet) 1)
  (if (>= (sprite-sheet-cell-timer sheet) (sprite-sheet-cell-max-time sheet))
      (progn (incf (sprite-sheet-current-cell sheet) 1)
	     (setf (sprite-sheet-cell-timer sheet) 0)))
  (if (>= (sprite-sheet-current-cell sheet) (* current-row (sprite-sheet-columns sheet)))
      (setf (sprite-sheet-current-cell sheet) (- (* current-row (sprite-sheet-columns sheet)) (sprite-sheet-columns sheet))))
  (draw-cell sheet (sprite-sheet-current-cell sheet)
	     x y
	     :width width :height height :color color :angle angle :center center :flip flip))

#|
======================================================================================
                            PALETTE & PALETTE ACCESSORIES
======================================================================================
|#
(defmacro define-palette (var fg bg fnt clear-color)
  `(defvar ,var (list ,fg ,bg ,fnt ,clear-color)))
(define-palette default-palette "d5aaaa" "866d5c" "fff3e3" "414f47")
(define-palette fields +brown+ +green+ +white+ +pastel-blue+)
(define-palette icy-wind "294552" "597884" "acc4ce" "9eb9b3")
(define-palette pink! +pastel-pink+  +black+  +pastel-pink+ "005a5c")
(defvar palettes '(default-palette fields icy-wind pink!))
(defvar palette nil)
(defvar current-palette 0)
(defvar fg-palette (nth 0 palette))
(defvar bg-palette (nth 1 palette))
(defvar font-palette (nth 2 palette))
(setf current-font-color font-palette)
(setf tamias-renderer-clear-color (nth 3 palette))

(defun update-palette ()
  (setf fg-palette (nth 0 palette)
	bg-palette (nth 1 palette)
	font-palette (nth 2 palette)
	current-font-color font-palette
	tamias-renderer-clear-color (nth 3 palette)))
(defmacro change-palette (var)
  `(let ((fg (nth 0 ,var))
	 (bg (nth 1 ,var))
	 (fnt (nth 2 ,var))
	 (clear-color (nth 3 ,var)))
     (if (stringp fg)
	 (setf fg (parse-hex-color fg :return-type 'list)))
     (if (stringp bg)
	 (setf bg (parse-hex-color bg :return-type 'list)))
     (if (stringp fnt)
	 (setf fnt (parse-hex-color fnt :return-type 'list)))
     (if (stringp clear-color)
	 (setf clear-color (parse-hex-color clear-color :return-type 'list)))
     (setf palette (list fg bg fnt clear-color))
     (update-palette)))

(change-palette default-palette)
