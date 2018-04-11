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
  (setf +font-sheet+ (sdl2-image:load-image "engine/Graphics Code/font.png"))
  ;;skeleton code
  (initialize-assets)
  )

(defun main ()
  (make-random-state)
  (sdl2:with-init (:everything)
    (setf default-window (sdl2:create-window :title title-name
					     :w (car (nth resolution resolution-list))
					     :h (cadr (nth resolution resolution-list))
					     :flags '(:shown)))
    (sdl2:with-renderer (default-renderer default-window :flags '(:accelerated))
      (sdl2-mixer:init :ogg)
      (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
      (setf renderer (sdl2:get-renderer default-window))
;;      (init-engine)
      (init)
      (start-main-menu-music (track-path main-menu-track))
      (sdl2:with-event-loop (:method :poll)
	(:keydown (:keysym keysym)
		  (keydown-check (sdl2:scancode keysym)))
	(:keyup (:keysym keysym)
		(keyup-check (sdl2:scancode keysym)))
	(:mousebuttondown (:button m-button)
			  (mouse-button-check m-button))
	(:mousebuttonup (:button m-button)
			(mouse-button-release-check m-button))
	(:mousemotion (:state b-state :x x :y y :xrel xrel :yrel yrel)
		      (mouse-move b-state x y xrel yrel))
	(:idle ()
	       (sdl2:set-render-draw-color renderer 0 0 0 255)
	       (sdl2:render-clear renderer)
	       (game-loop)
	       (sdl2:render-present renderer)
	       (sdl2:delay 33)
	       (gc :full t)
	       )
	(:quit ()
	       (quit-audio)
	       (kill-textures)
	       (sdl2:destroy-renderer renderer)
	       (sdl2:destroy-window default-window)
	       t)
	  ))))

(defun game-loop ()
  (test-music)
  (render-state)
  (if changing-state
      (process-changing-state))
  (process-loop)
  )

(defun kill-textures ()
  (setf buffers nil)
  (if +font-sheet+
      (progn (sdl2:free-surface +font-sheet+)
	     (setf +font-sheet+ nil)))
  )

(defun create-exec ()
  (sb-ext:save-lisp-and-die "main" :toplevel #'main :executable t)
  )
