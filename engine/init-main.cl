#|

starting sequence
(load "compile.cl")
(load-game :game-name) or (load-project :project-name)
or
(load-game "Game Name") or (load-project "Project Name")

|#

(defun init ()
  (if tamias:ttf-font
      (progn (sdl2-ttf:init)
      	     (setf tamias:ttf-font (sdl2-ttf:open-font tamias:ttf-font-path tamias:ttf-font-size)))
      (setf tamias:font (sdl2-image:load-image "engine/Graphics Code/font.png")))
  ;;skeleton code
  (initialize-assets))

(defun start-ttf-font ()
  (sdl2-ttf:init)
  (setf tamias:ttf-font (sdl2-ttf:open-font tamias:ttf-font-path tamias:ttf-font-size)))

(defun stop-ttf-font ()
  (if tamias:ttf-font
      (progn (sdl2-ttf:close-font tamias:ttf-font)
	     (setf tamias:ttf-font nil)
	     (sdl2-ttf:quit))))

(defmacro fps (target)
  `(progn (setf tamias:fps ,target)
	  (setf tamias:update-time (round (/ 1000 ,target)))))

(defun main ()
  (make-random-state)
  (sdl2:with-init (:everything)
    (setf tamias:default-window (sdl2:create-window :title tamias:title
					     :w (car (nth tamias:resolution tamias:resolution-list))
					     :h (cadr (nth tamias:resolution tamias:resolution-list))
					     :flags '(:shown)))
    (sdl2:with-renderer (tamias:renderer ;;default-renderer
			 tamias:default-window :flags '(:accelerated))
      (sdl2-mixer:init :ogg) ;;(sdl2-mixer:init :wave :ogg :mp3 :mod) As of right now, those are the 4 formats supported by SDL2. For the time being, I'm only supporting ogg for it's seeking support. I do have some semblance of plans to make my own audio server, but I may not end up doing it
      (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
;;      (setf tamias:renderer (sdl2:get-renderer tamias:default-window))
      (init)
      (sdl2:set-render-draw-blend-mode tamias:renderer 1) ;;allows for transparency
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
	       (if tamias:ttf-font
		   (progn (sdl2-ttf:close-font tamias:ttf-font)
			  (sdl2-ttf:quit)))
;;	       (sdl2:destroy-renderer tamias:renderer)
;;	       (sdl2:destroy-renderer default-renderer)
;;	       (sdl2:destroy-window tamias:default-window)
	       t)))))

(defun primary-loop ()
  (sdl2:set-render-draw-color tamias:renderer (car tamias:render-clear-color) (cadr tamias:render-clear-color) (caddr tamias:render-clear-color) 255)
  (sdl2:render-clear tamias:renderer)
  (if tamias:state
      (game-loop))
  (sdl2:render-present tamias:renderer)
  (if (> tamias:fps 0)
      (sdl2:delay tamias:update-time)))

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
	  ((sprite-sheeet tile-sheet) (sprite:free-sheet (eval (car **asset**))))))
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
