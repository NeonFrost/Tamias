#|
======================================================================================
                       PALETTE & PALETTE ACCESSORIES
======================================================================================
|#
(defmacro define-palette (var fg bg fnt clear-color)
  `(defvar ,var (list ,fg ,bg ,fnt ,clear-color)))
(define-palette default-palette "d5aaaa" "866d5c" "fff3e3" "414f47")
(define-palette fields tamias.colors:+brown+ tamias.colors:+green+ tamias.colors:+white+ tamias.colors:+pastel-blue+)
(define-palette icy-wind "294552" "597884" "acc4ce" "9eb9b3")
(define-palette pink! tamias.colors:+pastel-pink+  tamias.colors:+black+ tamias.colors:+pastel-pink+ "005a5c")
(defvar palettes '(default-palette fields icy-wind pink!))
(defvar palette nil)
(defvar current-palette 0)
(defvar fg-palette (nth 0 palette))
(defvar bg-palette (nth 1 palette))
(defvar font-palette (nth 2 palette))
(setf tamias:font-color font-palette)
(setf tamias:render-clear-color (nth 3 palette))

(defun update-palette ()
  (setf fg-palette (nth 0 palette)
	bg-palette (nth 1 palette)
	font-palette (nth 2 palette)
	tamias:font-color font-palette
	tamias:render-clear-color (nth 3 palette)))
(defun change-palette (var)
  (let ((fg (nth 0 var))
	(bg (nth 1 var))
	(fnt (nth 2 var))
	(clear-color (nth 3 var)))
    (if (stringp fg)
	(setf fg (tamias.string:parse-hex-color fg :return-type 'list)))
    (if (stringp bg)
	(setf bg (tamias.string:parse-hex-color bg :return-type 'list)))
    (if (stringp fnt)
	(setf fnt (tamias.string:parse-hex-color fnt :return-type 'list)))
    (if (stringp clear-color)
	(setf clear-color (tamias.string:parse-hex-color clear-color :return-type 'list)))
    (setf palette (list fg bg fnt clear-color))
    (update-palette)))

(change-palette default-palette)

(defpackage render
  (:use :cl)
  (:export draw-line
	   line
	   define-buffer
	   create-texture
	   with-rectangle
	   create-rectangle
	   free-rectangle
	   tex-blit
	   box
	   rectangle
	   reset-text-buffer
	   render-buffer
	   text-width
	   text-height
	   text-dimensions
	   render-string
	   text))
(in-package :render)
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
    (sdl2:set-render-draw-color tamias:renderer r g b a)
    (sdl2:render-draw-line tamias:renderer x y x2 y2)))

(defmacro line (x y x2 y2 &key (color '(255 255 255 255)))
  `(render:draw-line ,x ,y ,x2 ,y2 :color ,color))

(defmacro define-buffer (buffer)
  `(progn (defvar ,buffer nil)
	  (push ,buffer tamias:buffers)))

(defun create-texture (&key (format sdl2:+pixelformat-rgba8888+) (access 0) (width 16) (height 16) (color tamias.colors:+white+))
  (sdl2:set-render-draw-color tamias:renderer (car color) (cadr color) (caddr color) (cadddr color))
  (sdl2:render-clear tamias:renderer)
  (sdl2:create-texture tamias:renderer format access width height))

(defmacro with-rectangle (name rect-parameters &body body)
  `(let ((,name (sdl2:make-rect (car ,rect-parameters)
				(cadr ,rect-parameters)
				(caddr ,rect-parameters)
				(cadddr ,rect-parameters))))
     ,@body
     (sdl2:free-rect ,name)))
#|
usage: (with-rectangle bat-rect (list (bat-x bat) (bat-y bat) (bat-width bat) (bat-height bat))
(render:rectangle bat-rect))
|#

(defmacro create-rectangle (rect-vals)
  `(sdl2:make-rect (car ,rect-vals)
		   (cadr ,rect-vals)
		   (caddr ,rect-vals)
		   (cadddr ,rect-vals)))
#|
usage: (defun some-func ()
          (let ((my-rect (create-rectangle '(x y width height))))
            (render:rectangle my-rect)
            (render:free-rectangle my-rect)))
|#
(defmacro free-rectangle (rect)
  `(sdl2:free-rect ,rect))

(defmacro tex-blit (tex &key src dest color angle center (flip :none))
  `(progn (if ,color
	      (progn (sdl2:set-texture-color-mod ,tex (car ,color) (cadr ,color) (caddr ,color))
		     (if (> (length ,color) 3)
			 (sdl2:set-texture-alpha-mod ,tex (nth 3 ,color)))))
	  (if (not ,src)
	      (let ((src (sdl2:make-rect 0 0 (sdl2:texture-width ,tex) (sdl2:texture-height ,tex))))
		(sdl2:render-copy-ex tamias:renderer
				     ,tex
				     :source-rect src
				     :dest-rect ,dest
				     :angle ,angle
				     :center ,center
				     :flip (list ,flip))
		(sdl2:free-rect src)
		(sdl2:free-rect ,dest))
	      (progn (sdl2:render-copy-ex tamias:renderer
					  ,tex
					  :source-rect ,src
					  :dest-rect ,dest
					  :angle ,angle
					  :center ,center
					  :flip (list ,flip))
		     (sdl2:free-rect ,src)
		     (sdl2:free-rect ,dest))
	      )))

(defun box (x y w h &key (color '(255 255 255 255)) (filled t))
  (let* ((r (car color))
	 (g (cadr color))
	 (b (caddr color))
	 (a (or (cadddr color)
		255))
	 (rect (sdl2:make-rect x y w h))) ;; I still want to figure out a way to better manage memory with Tamias, currently, a rectangle is made and then freed every frame
    ;;set the render draw color to the color
    (sdl2:set-render-draw-color tamias:renderer r g b a)
    (if filled
	(sdl2:render-fill-rect tamias:renderer rect) ;;fills the area of x -> w and y -> h with color (default white)
	(sdl2:render-draw-rect tamias:renderer rect)) ;;draws 4 lines, x -> , x -> w, y -> w, h -> w with color (default white)
    (sdl2:free-rect rect))) ;;frees memory taken up by the rect. Not doing this will cause a segfault

(defun rectangle (rect &key (color '(255 255 255 255)) filled)
  (box (car rect) (cadr rect)
       (caddr rect) (nth 3 rect)
       :color color :filled filled))


(defun pixel (x y &key (color '(255 255 255 255)))
  (box x y 1 1 :color color))

#|(defun calculate-arc-slope (x1 y1 x2 y2)
  (let ((rise (- y2 y1))
	(run (- x2 x1))
	slope-case)
    (if (< rise 0)
	;;going up
	    ;;going left
	    ;;going right
	;;going down
	    ;;going left
	    ;;going right
	)
    (case slope-case
      (SE );;midpoint C = + (/ run 2) x1 :: + (/ rise 2) y1
      (SW );; C = + (/ run 2) x1 :: + (/ rise 2) y1
      (NW );; C = + (/ run 2) x1 :: + (/ rise 2) y1
      (NE );; C = + (/ run 2) x1 :: + (/ rise 2) y1
      ) ;;NOTE: I may be able to just use C = ... for the midpoint regardless of direction
    )
  )

(defun circle (cx cy r &key filled)
  ;;draw 4 arcs
  (let ((x1 cx)
	(x2 (+ cx r))
	(x3 (- cx r))
	(y1 cy)
	(y2 (- cy r))
	(y3 (+ cy r)))
    ))

|#
#|
In the end, the goal is to get a list of points, and to use that list to draw a circle
Interestingly, because of how I'm getting the "slope", I don't need to reverse the list of the slope
Instead, I can apply that list in the opposite direction
I.e., if a list is going right-down, +X +Y then when it would be going up-left, -Y -X
  |#  
    ;;draw order: x1::y2  x2::y1  x1::y3  x3::y1
    ;;filled uses lines, not-filled goes along a list of points and fills in those points
    ;;ignore points that are past x + r, x -r, y + r and y - r
    ;;or x - r <= x <= x + r and y -r <= y <= y + r
    ;;general idea is that from x1::y2 to x2::y1 would be a "long less-long middle less-middle one" pattern, then reverse that pattern "one ... middle...long"
    ;;Where the x slope goes down, while y slope does not change, so x begins as 6, then 5, 4 and so on. then the pattern that started with x is reversed and is applied to y
    ;;while the x slope "=" 1, just like y.
    ;;so, pseudocode:
    #|
    while x-slope > 0
    loop below x-slope
    do incf x-pixel
       collect pair x-pixel y-pixel
       END
    incf y-pixel
    decf x-slope
    END REP
    |#
    
  
#|

Another methodology for drawing a curved arc:
First get the slope between two points in a rectangle
We'll call these two points, which are the beginning and end of an arc, A and B
After getting the slope of A and B, determine the midpoint, which will be called C
Create a new point D that is a straight line from C, so as to be perpendicular to line AB
Determine the midpoint of CD, E
Determine slope of AE, where the Angle of CAD is 45 degrees
Divide the slope of X of AE by the y slope of AE
This is the amount of X steps required to create a curved arc in the X direction
Upon completion of the X steps, switch to Y steps
Determine an evenly spaced out step amount, where it is something like (4 2 1)
Move 4 in the x direction, rendering a pixel per point
Move 1 in the y direction, render
Move 2 in X, render
Move 1 in y
Move 1 in X
Reverse the list, and switch to Y steps
upon completion of the X and Y steps, a curved line will be drawn

Drawing a circle:
Arc method, but upon completion of Arc 1, reverse X direction
Arc 2, reverse y
Arc 3, reverse X
Arc 4 will be the completed circle

Example:
        A----------------------------D
         \--\  	         	 --/
	  \  -----\   	      --/
	   \	   ---\     -/
            \	       ---E/
	     \	      --/|
	      \	   --/	 |
	       \C-/   	/
                \     	|
		 \    	|
		  \    /
		   \   |
		    \ /
		     \|
                      B
|#


(defmacro reset-text-buffer (buffer)
  `(if ,buffer
       (progn (sdl2:destroy-texture ,buffer)
	      (setf ,buffer nil))))

#|
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
    (sdl2:render-copy tamias:renderer
		      buffer
		      :source-rect src				    
		      :dest-rect dest)
    (sdl2:free-rect src)
    (sdl2:free-rect dest)))

This was for the roguelike I was/am working on
|#

(defun text-width (text)
  (* (length text) (car tamias.string:character-size))
  )
(defun text-height (text)
  (let ((new-line-count (1+ (count #\newline text))))
    (* new-line-count (cadr tamias.string:character-size))))
(defun text-dimensions (text)
  (if (symbolp text)
      (setf text (write-to-string text)))
  (if (stringp text)
      (let ((width (text-width text))
	    (height (text-height text)))
	(list width height))
      (list 0 0)))


(defun render-string (str x y &key width height string-width string-height (rotate 0) (color tamias:font-color) anti-alias)
  (let ((buffer-width (or width
	       (if (find #\newline str)
		   (* (position #\newline str) (car tamias.string:character-size)))
	       (* (length str) (car tamias.string:character-size))))
	(buffer-height (or height
	       (* (1+ (count #\newline str)) (cadr tamias.string:character-size))))
	(width (or width
		   string-width
		   0))
	(height (or height
		    string-height
		    0)))
    #|    (if (find #\newline str)
    (if (> (* (position #\newline str) (car tamias.string:character-size)) w)
    (setf w (* (position #\newline str) (car tamias.string:character-size)))))|#
    (if (> (length str) 0)
	(let* ((string-buffer (tamias.string:create-text-buffer str
								:width buffer-width
								:height buffer-height))
	       (source-width (if (> width (sdl2:texture-width string-buffer))
				 width
				 (sdl2:texture-width string-buffer)))
	       (source-height (if (> height (sdl2:texture-height string-buffer))
				  height
				  (sdl2:texture-height string-buffer)))
	       (destination-width (or string-width
				      source-width))
	       (destination-height (or string-height
				       source-height)))
	  (tex-blit string-buffer :src (create-rectangle (list 0 0 source-width source-height))
		    :dest (create-rectangle (list x y destination-width destination-height))
		    :color color
		    :angle rotate)
	  (if anti-alias
	      (let ((alpha-color (list (car color) (cadr color) (caddr color) 125)))
		(tex-blit string-buffer :src (create-rectangle (list 0 0 source-width source-height))
			  :dest (create-rectangle (list (1- x) y destination-width destination-height))
			  :color alpha-color
			  :angle rotate)
		(tex-blit string-buffer :src (create-rectangle (list 0 0 source-width source-height))
			  :dest (create-rectangle (list (1+ x) y destination-width destination-height))
			  :color alpha-color
			  :angle rotate)
		(tex-blit string-buffer :src (create-rectangle (list 0 0 source-width source-height))
			  :dest (create-rectangle (list x (1- y) destination-width destination-height))
			  :color alpha-color
			  :angle rotate)
		(tex-blit string-buffer :src (create-rectangle (list 0 0 source-width source-height))
			  :dest (create-rectangle (list x (1+ y) destination-width destination-height))
			  :color alpha-color
			  :angle rotate)))
	  (reset-text-buffer string-buffer)))))

(defun determine-string-width-nlc (str)
  (let ((h (count #\newline str))
	(longest (length str))
	(current-line-length 0))
    (if (> h 0)
	(loop for idx below (length str)
	   do (if (char= (aref str idx) #\newline)
		  (if (> current-line-length longest)
		      (setf longest current-line-length
			    current-line-length 0)
		      (setf current-line-length 0))
		  (incf current-line-length)))
	(incf h))
    ;;ensures that longest does not return 0, if there is no newline character
    (if (> current-line-length longest)
	(setf longest current-line-length))
    (values longest h)))

(defun text (str x y &key width height (rotate 0) (color tamias:font-color) scale origin-is-center)
  (let ((src-width 0)
	(src-height 0))
    (if tamias:ttf-font
	(let* ((str-surf (sdl2-ttf:render-text-solid tamias:ttf-font str
						     255 255 255 0))
	       (str-tex (sdl2:create-texture-from-surface tamias:renderer
							  str-surf)))
	  (sdl2:free-surface str-surf)
	  (tex-blit str-tex :dest (create-rectangle (list x y width height))
		    :color color
		    :angle rotate)
	  (sdl2:destroy-texture str-tex))
	(progn
	  ;;(setf (values text-width newline-count) (determine-string-width-nlc str)))
	  (let ((dim (text-dimensions str)))
	    (setf src-width (car dim)
		  src-height (cadr dim)))
      	  (if (not width)
	      (setf width src-width))
	  (if (not height)
	      (setf height src-height))
	  (if scale
	      (let ((string-width width)
		    (string-height height))
		;; stretches the text to the scale
		(setf width (round (* string-width scale))
		      height (round (* string-height scale))
		      x (- x (round (/ width 2)))
		      y (- y (round (/ height 2))))
		))
	  (if origin-is-center
	      (progn (decf x (round (/ width 2)))
		     (decf y (round (/ height 2)))))
	;;(render-string str x y :width width :height height :dest-width dest-width :dest-height dest-height :rotate rotate :color color :anti-alias t))
	;;      (render-string str x y :width width :height height :dest-width dest-width :dest-height dest-height :rotate rotate :color color :anti-alias t)))
	  (render-string str x y :width src-width :height src-height :string-width width :string-height height :rotate rotate :color color :anti-alias t)))))
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

(defpackage :sprite
  (:use :cl)
  (:export make-sheet
	   sheet-file
	   sheet-width
	   sheet-height
	   sheet-cells
	   sheet-rows
	   sheet-columns
	   sheet-surface
	   sheet-texture
	   sheet-cell-timer
	   sheet-cell-max-time
	   sheet-current-cell
	   optimize-sheet
	   set-sheet-width
	   set-sheet-height
	   set-cells
	   set-sheet-surface
	   load-sheet
	   defsheet
	   cell
	   tile
	   free-sheet
	   animate))
(in-package :sprite)

(defstruct sheet
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
  `(setf (sprite:sheet-texture ,var) (sdl2:create-texture-from-surface tamias:renderer (sheet-surface ,var))))

(defmacro set-sheet-width (sheet width)
  `(setf (sprite:sheet-width ,sheet) ,width))

(defmacro set-sheet-height (sheet height)
  `(setf (sprite:sheet-height ,sheet) ,height))

(defmacro set-cells (sheet tile-size)
  `(let ((rows (/ (sprite:sheet-height ,sheet) (cadr ,tile-size)))
	 (columns (/ (sprite:sheet-width ,sheet) (car ,tile-size)))
	 (cells (loop for y below (sprite:sheet-height ,sheet) by (cadr ,tile-size)
		   append (loop for x below (sprite:sheet-width ,sheet) by (car ,tile-size)
			     collect (list x y (car ,tile-size) (cadr ,tile-size))))))
     (setf (sprite:sheet-rows ,sheet) rows
	   (sprite:sheet-columns ,sheet) columns
	   (sprite:sheet-cells ,sheet) cells)
     ))

(defmacro set-sheet-surface (sheet surface)
  `(setf (sprite:sheet-surface ,sheet) ,surface))

(defmacro load-sheet (sheet cell-size)
  `(let* ((filename (sheet-file ,sheet))
	  (surface (sdl2-image:load-image filename)))
     (set-sheet-height ,sheet (sdl2:surface-height surface))
     (set-sheet-width ,sheet (sdl2:surface-width surface))
     (set-cells ,sheet ,cell-size)
     (set-sheet-surface ,sheet surface)
     (optimize-sheet ,sheet)))

(defmacro defsheet (entity file cell-size)
  `(progn (setf (entity-sheet-surface ,entity) (make-sheet :file ,file))
	  (load-sheet (entity-sheet-surface ,entity) ,cell-size)))
(defmacro free-sheet (sheet)
  `(progn (sdl2:destroy-texture (sheet-texture ,sheet))
	  (setf ,sheet nil)))


#|
usage:
(defsheet bat "game/gfx/bat-sheet.png" '(cell-width cell-height))
then (entity-sheet bat) for the bat's sheet
also, the idea is that "entity" is a generalized structure in which all of your entity structs
are based on, mainly to help in reducing necessary labour time
|#

(in-package :render)

(defun cell (sheet cell x y &key width height color (angle 0) center (flip :none))
  (let* ((cells (sprite:sheet-cells sheet))
	 (src-rect (sdl2:make-rect (nth 0 (nth cell cells))
				   (nth 1 (nth cell cells))
				   (nth 2 (nth cell cells))
				   (nth 3 (nth cell cells))))
	 (tsx (nth 2 (nth cell cells)))
	 (tsy (nth 3 (nth cell cells)))
	 (dest-rect (sdl2:make-rect x
				    y
				    (if width
					width
					tsx)
				    (if height
					height
					tsy)))
	 (flip (if (or (eq flip :none)
		       (eq flip :horizontal)
		       (eq flip :vertical))
		   flip
		   :none))
	 (center (if center
		     center
		     nil))
	 (angle (if (not (or (integerp angle)
			     (floatp angle)))
		    0
		    angle)))
    (tex-blit (sprite:sheet-texture sheet)
	      :src src-rect :dest dest-rect
	      :color color :angle angle :center center :flip flip)))
(defun tile (sheet cell x y &key width height color (angle 0) center (flip :none))
  (cell sheet cell
	x y
	:width width :height height :color color :angle angle :center center :flip flip))

(defun animate (sheet x y &key width height (current-row 1) color (angle 0) center (flip :none))
  (incf (sprite:sheet-cell-timer sheet) 1)
  (if (>= (sprite:sheet-cell-timer sheet) (sprite:sheet-cell-max-time sheet))
      (progn (incf (sprite:sheet-current-cell sheet) 1)
	     (setf (sprite:sheet-cell-timer sheet) 0)))
  (if (>= (sprite:sheet-current-cell sheet) (* current-row (sprite:sheet-columns sheet)))
      (setf (sprite:sheet-current-cell sheet) (- (* current-row (sprite:sheet-columns sheet)) (sprite:sheet-columns sheet))))
  (cell sheet (sprite:sheet-current-cell sheet)
	     x y
	     :width width :height height :color color :angle angle :center center :flip flip))

