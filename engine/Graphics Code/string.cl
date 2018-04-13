#|
On rendering strings, either use a ttf file and use a 'buffer' with the strings (rendering a string through sdl-ttf requires many calculations and procedures to take place, resulting in /massive/ slowdown if done every frame update) or use a tile sheet with 256 characters on it, take the ascii character code of each character of the string (i.e. a = 58), divide by 16, with the quotient being used for the row and the remainder being used for the column (60/16 = 3 12/16 = R:3 C:12) cell and then render it to either a buffer (recommended) or to the renderer
|#
(defvar +font-sheet+ nil)
(defvar character-size '(16 16))

(defmacro blit (src-surface src-rect dest-surface dest-rect)
  `(sdl2:blit-surface ,src-surface ,src-rect ,dest-surface ,dest-rect))

(defun combine-strings (str &rest strings)
  "Used on strings that just need to be concatenated together, but not inserting a newline at the end of each of them."
  (loop for s in strings
     do (setf str (concatenate 'string str s)))
  str)

(defun start-string (str &rest strs)
  (loop for s in strs
       do (setf str (with-output-to-string (stream)
		      (write-string str stream)
		      (terpri stream)
		      ))
       (setf str (concatenate 'string str s)))
  str)

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
    (blit +font-sheet+ src-rect temporary-surface tmp-rect)
    (sdl2:set-color-mod temporary-surface (car color) (cadr color) (caddr color))
    (blit temporary-surface tmp-rect buffer dest-rect)
    (sdl2:free-surface temporary-surface)
    (sdl2:free-rect src-rect)
    (sdl2:free-rect dest-rect)
    (sdl2:free-rect tmp-rect)
    ))

(defun create-text-buffer (string &key (width 256) (height 256) to-texture (string-case 'array))
  (let ((buff (sdl2:create-rgb-surface width height 32))
	(cell-row 0)
	(cell-column 0)
	(mod-x -1)
	(mod-y 0)
	(texture nil)
	(temp-value 0)
	(x 0)
	(y 0))
    (loop for n below (length string)
       do (setf (values cell-row cell-column) (truncate (char-code (aref string n)) 16))
       ;;In order to display the text-strings correctly, the code must check for a newline character before n and use that for the divisor,
       ;;or use the position of #\Newline if there is no newline character before n
	 (if (find #\NewLine string)
	     (case string-case
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
	     (if (eq string-case 'text)
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
	       (if (eq string-case 'array)
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
					       :color (list 255 255 255 255)))
	       )))
    (sdl2:set-color-key buff 1 0)
    (if to-texture
	(progn (setf texture (sdl2:create-texture-from-surface renderer buff))
	       (sdl2:free-surface buff)
	       (setf buff nil)
	       texture)
	buff)))

(defun font-to-surface (font)
  ;;to be implemented: making ttf fonts easier and better to use
  ;;Idea: loop through each ascii character (loop for char below 256) and push the rendered character to a surface that is then used as the font-sheet for the program
  )
