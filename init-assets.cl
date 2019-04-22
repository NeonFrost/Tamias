(defvar **tamias-assets-functions** nil)
(defvar **tamias-assets** nil)

(defmacro add-asset (var-name &key asset-type file loop-point cell-size)
  `(progn (defvar ,var-name nil)
	  (push (list ',var-name ',asset-type ,file ,loop-point ,cell-size) **tamias-assets**)))
(defmacro add-asset-function (&body body) ;;add a more complicated add-asset macro
  `(push ',@body **tamias-assets-functions**))

(defun initialize-assets ()
  ;;(start-music main-menu-track)
  ;;assets go here, like music, tile-sheets, sprite-sheets, fonts, etc.
  (loop for **asset** in **tamias-assets-functions**
     do (eval **asset**))
  (loop for **asset** in **tamias-assets**
     do (let ((var (car **asset**)))
	  (case (cadr **asset**)
	    ((image texture) (set var (sdl2:create-texture-from-surface renderer (sdl2-image:load-image (caddr **asset**))))) ;;sets the variable to a loaded image from the file
	    ((sprite-sheet tile-sheet) (set var (make-sprite-sheet :file (caddr **asset**)))
	     (load-sheet (eval var) (nth 4 **asset**))) ;;loads the sprite sheet to variable
	    ((track music) (set var (make-track :path (caddr **asset**) :loop-point (nth 3 **asset**))))
	    ((sample sound) (set var (make-sample :path (caddr **asset**))))))))
