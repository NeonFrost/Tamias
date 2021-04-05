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
     do (let ((var (car **asset**))
	      (file-success (caddr **asset**)))
	  (case (cadr **asset**)
	    ((image texture) (if file-success
				 (set var (sdl2:create-texture-from-surface tamias:renderer (sdl2-image:load-image (caddr **asset**))))
				 (tamias:console.add-message (concatenate 'string "Unable to open image/texture " (caddr **asset**))))) ;;sets the variable to a loaded image from the file
	    ((sprite-sheet tile-sheet) (if file-success
					   (progn (set var (sprite:make-sheet :file (caddr **asset**)))
						  (sprite:load-sheet (eval var) (nth 4 **asset**))) ;;loads the sprite sheet to variable
					   (tamias:console.add-message (concatenate 'string "Unable to open Sprite sheet or Tile sheet " (caddr **asset**)))))
	    ((track music) (if file-success
			       (set var (make-track :path (caddr **asset**) :loop-point (nth 3 **asset**)))
			       (tamias:console.add-message (concatenate 'string "Unable to open Music File " (caddr **asset**)))))
	    ((sample sound) (if file-success
				(set var (make-sample :path (caddr **asset**)))
				(tamias:console.add-message (concatenate 'string "Unable to open Sound File " (caddr **asset**)))))))))
