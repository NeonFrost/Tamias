#|
starting sequence
(asdf:load-system "Kitchen Craze") ;;note: in future projects, use hyphens always, no spaces

(ql:quickload :sdl2) (ql:quickload :sdl2-image) (ql:quickload :sdl2-mixer) 
(load "Main.lisp") (main)

(proclaim '(optimize (speed 3) (debug 0)))
|#
#|(defun init-engine ()
  (load "engine.cl")
)|#

(defun init ()
  (setf tamias:font (sdl2-image:load-image "engine/Graphics Code/font.png"))
  ;;skeleton code
  (initialize-assets))

(defmacro fps (target)
  `(progn (setf tamias:fps ,target)
	  (setf tamias:update-time (round (/ 1000 ,target)))))

(defun main ()
  (make-random-state)
  (sdl2:with-init (:everything)
    (setf tamias:default-window (sdl2:create-window :title tamias:game-title
					     :w (car (nth tamias:resolution tamias:resolution-list))
					     :h (cadr (nth tamias:resolution tamias:resolution-list))
					     :flags '(:shown)))
    (sdl2:with-renderer (default-renderer tamias:default-window :flags '(:accelerated))
      ;;add in conditional after Oreortyx is finished, ie (if not probe-file oreortyx, blah blah)
      (sdl2-mixer:init :ogg)
      (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
      (setf tamias:renderer (sdl2:get-renderer tamias:default-window))
      (init)
      (sdl2:set-render-draw-blend-mode tamias:renderer 1)
      (update-window-size)
      (sdl2:stop-text-input)
      (sdl2:with-event-loop (:method :poll)
	(:keydown (:keysym keysym)
		  (keydown-check (sdl2:get-key-name (sdl2:sym-value keysym))))
	(:keyup (:keysym keysym)
		(keyup-check (sdl2:get-key-name (sdl2:sym-value keysym))))
	(:mousebuttondown (:button m-button)
			  (mouse-button-check m-button))
	(:mousebuttonup (:button m-button)
			(mouse-button-release-check m-button))
	(:mousemotion (:state b-state :x x :y y :xrel xrel :yrel yrel)
		      (mouse-move b-state x y xrel yrel))
	(:textinput (:text text)
		    (handle-text-input text))
	(:idle ()
	       (primary-loop)
	       (gc :full t))
	(:quit ()
	       (quit-audio)
	       (kill-textures)
	       (sdl2:destroy-renderer tamias:renderer)
	       (sdl2:destroy-window tamias:default-window)
	       t)))))

(defun primary-loop ()
  (sdl2:set-render-draw-color tamias:renderer (car tamias:render-clear-color) (cadr tamias:render-clear-color) (caddr tamias:render-clear-color) 255)
  (sdl2:render-clear tamias:renderer)
  (game-loop)
  (sdl2:render-present tamias:renderer)
  (sdl2:delay tamias:update-time))

(defun game-loop ()
  (test-music)
  (render-state)
  (if tamias:changing-state
      (process-changing-state))
  (process-loop))

(defun kill-textures ()
  (setf tamias:buffers nil)
  (loop for **asset** in **tamias-assets**
     do (case (cadr **asset**)
	  ((image texture) (sdl2:destroy-texture (eval (car **asset**))))
	  ((sprite-sheeet tile-sheet) (free-sheet (eval (car **asset**))))))
  (if tamias:font
      (progn (sdl2:free-surface tamias:font)
	     (setf tamias:font nil))))

(defun create-exec (&key linking (name "main") (system :tamias))
  (if (not name)
      (setf name "main"))
  (if (not (stringp name))
      (setf name (write-to-string name)))
  (case linking
    (image (asdf:load-system :cffi-grovel)
	   (asdf:operate :static-image-op system))
    (app (asdf:make system))
    (otherwise (sb-ext:save-lisp-and-die name :toplevel #'main :executable t))))
