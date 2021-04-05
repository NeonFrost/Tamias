#|
On rendering strings, either use a ttf file and use a 'buffer' with the strings (rendering a string through sdl-ttf requires many calculations and procedures to take place, resulting in /massive/ slowdown if done every frame update) or use a tile sheet with 256 characters on it, take the ascii character code of each character of the string (i.e. a = 58), divide by 16, with the quotient being used for the row and the remainder being used for the column (60/16 = 3 12/16 = R:3 C:12) cell and then render it to either a buffer (recommended) or to the renderer
|#
(defpackage tamias.string
  (:use :cl)
  (:export character-size
	   create-text-buffer
	   ascii-to-string
	   parse-hex-color
	   combine-strings))
(in-package :tamias.string)
(defvar character-size '(16 16))

(defun ascii-to-string (code)
  (if (integerp code)
      (concatenate 'string "" (list (code-char code)))
      (concatenate 'string "" (string code))))

(defmacro blit (src-surface src-rect dest-surface dest-rect)
  `(sdl2:blit-surface ,src-surface ,src-rect ,dest-surface ,dest-rect))

(defun combine-strings (str &rest strings)
  "Used on strings that just need to be concatenated together, but not inserting a newline at the end of each of them."
  (loop for s in strings
     do (if (not (stringp s))
	    (setf s (write-to-string s)))
       (setf str (concatenate 'string str s)))
  str)

#|
(defun start-string (str &rest strs)
  (loop for s in strs
       do (setf str (with-output-to-string (stream)
		      (write-string str stream)
		      (terpri stream)
		      ))
       (setf str (concatenate 'string str s)))
  str)
|#
(defun render-character-to-buffer (cell x y buffer &key (color (list 255 255 255)))
  (let ((src-rect (sdl2:make-rect (* (cadr cell) (car character-size))
				  (* (car cell) (cadr character-size))
				  (car character-size)
				  (cadr character-size)))
	(dest-rect (sdl2:make-rect x
				   y
				   (car character-size)
				   (cadr character-size)))
	(temporary-surface (sdl2:create-rgb-surface (car character-size) (cadr character-size) 32))
	(tmp-rect (sdl2:make-rect 0 0 (car character-size) (cadr character-size))))
    (blit tamias:font src-rect temporary-surface tmp-rect)
    (sdl2:set-color-mod temporary-surface (car color) (cadr color) (caddr color))
    (blit temporary-surface tmp-rect buffer dest-rect)
    (sdl2:free-surface temporary-surface)
    (sdl2:free-rect src-rect)
    (sdl2:free-rect dest-rect)
    (sdl2:free-rect tmp-rect)
    ))



(defun create-text-buffer (string &key (width 256) (height 256) to-surface);;yeah, this needs to be changed significantly
  ;;some of the code below, that will be gone by the time somebody else reads this, was being used for a rogue-like I was developing
  ;;or rather still developing, but not very much has been developed for a while
  (let ((buff (sdl2:create-rgb-surface width height 32))
	(cell-row 0)
	(cell-column 0)
	(mod-x -1)
	(mod-y 0)
	(texture nil)
	(x 0)
	(y 0))
    (loop for n below (length string)
       do (setf (values cell-row cell-column) (truncate (char-code (aref string n)) 16))
	 (if (find #\NewLine string)
	      (setf (values mod-y mod-x) (truncate n (1+ (position #\Newline string)))))
	 (incf mod-x 1)
	 (if (and (eq (mod n (/ width (car character-size))) 0)
		  (> mod-x 0))
	     (progn (setf mod-x 0)
		    (incf mod-y 1)))
	 (render-character-to-buffer (list cell-row cell-column)
				     (+ x (* mod-x (car character-size)))
				     (+ y (* mod-y (cadr character-size)))
				     buff
				     :color (list 255 255 255)))
    (sdl2:set-color-key buff 1 0)
    (if to-surface
	buff
	(progn (setf texture (sdl2:create-texture-from-surface tamias:renderer buff))
	       (sdl2:free-surface buff)
	       (setf buff nil)
	       texture))))

(defun font-to-surface (font)
  ;;to be implemented: making ttf fonts easier and better to use
  ;;Idea: loop through each ascii character (loop for char below 256) and push the rendered character to a surface that is then used as the font-sheet for the program
  )
;;this will need to be updated to support utf-8

(defmacro delimit-inclusive (limiter str &key (modifier 0))
  `(if (characterp ,limiter)
       (subseq ,str (+ (position ,limiter ,str) ,modifier) (length ,str))
       (if (integerp ,limiter)
	   (subseq ,str ,limiter)
	   (let ((dl (aref ,limiter 0)))
	     (subseq ,str (+ (position dl ,str) ,modifier) (length ,str))
	     ))))

(defmacro delimit-exclusive (limiter str &key (modifier 0))
  `(if (characterp ,limiter)
       (subseq ,str (+ (1+ (position ,limiter ,str)) ,modifier) (length ,str))
       (if (integerp ,limiter)
	   (subseq ,str (1+ ,limiter))
	   (let ((dl (aref ,limiter 0)))
	     (subseq ,str (+ (1+ (position dl ,str)) ,modifier) (length ,str))
	     ))))

(defmacro delimit-to-inclusive (lower-limit upper-limit str &key (modifier 0))
  `(if (and (characterp ,lower-limit)
	    (characterp ,upper-limit))
       (subseq ,str (+ (position ,lower-limit ,str) ,modifier) (position ,upper-limit ,str))
       (let ((dl (if (characterp ,lower-limit)
		     ,lower-limit
		     (if (integerp ,lower-limit)
			 (+ ,lower-limit ,modifier)
			 (aref ,lower-limit 0))))
	     (ul (if (characterp ,upper-limit)
		     ,upper-limit
		     (if (integerp ,upper-limit)
			 ,upper-limit
			 (aref ,upper-limit 0)))))
	 (if (and (integerp dl)
		  (integerp ul))
	     (subseq ,str dl ul)
	     (if (integerp dl)
		 (subseq ,str dl (position ul ,str))
		 (if (integerp ul)
		     (subseq ,str (+ (position dl ,str) ,modifier) ul)
		     (subseq ,str (+ (position dl ,str) ,modifier) (position ul ,str))))))))

(defmacro delimit-to-exclusive (lower-limit upper-limit str &key (modifier 0))
  `(if (and (characterp ,lower-limit)
	    (characterp ,upper-limit))
       (subseq ,str (+ (1+ (position ,lower-limit ,str)) ,modifier) (position ,upper-limit ,str))	   
       (let ((dl (if (characterp ,lower-limit)
		     ,lower-limit
		     (if (integerp ,lower-limit)
			 (1+ (+ ,lower-limit ,modifier))
			 (aref ,lower-limit 0))))
	     (ul (if (characterp ,upper-limit)
		     ,upper-limit
		     (if (integerp ,upper-limit)
			 ,upper-limit
			 (aref ,upper-limit 0)))))
	 (princ (integerp dl)) (princ (integerp ul))
	 (if (and (integerp dl)
		  (integerp ul))
	     (subseq ,str dl ul)
	     (if (integerp dl)
		 (subseq ,str dl (position ul ,str))
		 (if (integerp ul)
		     (subseq ,str (+ (1+ (position dl ,str)) ,modifier) ul)
		     (subseq ,str (+ (1+ (position dl ,str)) ,modifier) (position ul ,str))))))))

(defun parse-hex-color (str &key (return-type 'values))
  (if (eq (aref str 0) #\#)
      (setf str (subseq str 1)))
  (let ((r (delimit-to-inclusive 0 2 str))
	(g (delimit-to-inclusive 2 4 str))
	(b (delimit-inclusive 4 str)))
    (setf r (parse-integer r :radix 16)
	  g (parse-integer g :radix 16)
	  b (parse-integer b :radix 16))
    (case return-type
      (values (values r g b 255))
      (list (list r g b 255))
      (otherwise (values r g b 255)))))
