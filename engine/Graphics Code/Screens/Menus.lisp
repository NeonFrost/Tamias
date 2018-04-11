(defstruct menu
  x
  x-equation
  y
  y-equation
  width
  width-equation
  height
  height-equation
  border-color
  fill-color)
(defvar menus nil)
(defun draw-menu (menu)
  (let ((rect1 (sdl2:make-rect (- (menu-x menu) 6) (- (menu-y menu) 6)
			       (+ (menu-width menu) 12) (+ (menu-height menu) 12)))
	(rect2 (sdl2:make-rect (menu-x menu) (menu-y menu)
			       (menu-width menu) (menu-height menu)))
	(border-color (menu-border-color menu))
	(fill-color (menu-fill-color menu)))
    (sdl2:set-render-draw-color renderer (car border-color) (cadr border-color) (caddr border-color) (cadddr border-color))
    (sdl2:render-fill-rect renderer rect1)
    (sdl2:set-render-draw-color renderer (car fill-color) (cadr fill-color) (caddr fill-color) (cadddr fill-color))
    (sdl2:render-fill-rect renderer rect2)
    (sdl2:free-rect rect1)
    (sdl2:free-rect rect2)))

(defmacro define-menu (name screen-menus x y w h border-color fill-color)
  `(progn #|(if (find (quote ,name) ,screen-menus)
	      (progn (setf ,screen-menus (remove (quote ,name) ,screen-menus))
		     (setf menus (remove (quote ,name) menus)))|#
     (defvar ,name (make-menu :x (+ ,x 6) :x-equation `,',x
			      :y (+ ,y 6) :y-equation `,',y
			      :width (- ,w 12) :width-equation `,',w
			      :height (- ,h 12) :height-equation `,',h
			      :border-color ,border-color :fill-color ,fill-color))
     (push (quote ,name) ,screen-menus)
     (push ,name menus)
     (defun ,name (menu)
       (draw-menu menu))))
(defmacro define-screen (name menus)
  `(defun ,name ()
     (loop for menu in ,menus
	do (funcall menu (eval menu)))))

(defun update-window-size ()
  (sdl2:set-window-size default-window *screen-width* *screen-height*)
  (loop for menu in menus
     do (setf (menu-x menu) (+ (eval (menu-x-equation menu)) 6)
	      (menu-y menu) (+ (eval (menu-y-equation menu)) 6)
	      (menu-width menu) (- (eval (menu-width-equation menu)) 12)
	      (menu-height menu) (- (eval (menu-height-equation menu)) 12))))

;;;;Usage: (define-menu stats-menu room-menus (- *sw* 128) 0 128 (- *sh* 128) '(127 127 127 127) +yellow-zircon+)
;;;;(define-screen room-screen room-menus)
;;;;(case state (battle (case sub-state (top (top-battle-screen)))))
