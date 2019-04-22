#|
This will be an attempt to create a demo of SNES style layers
|#

(define-state layers-demo)
(setf state 'layers-demo)

(defvar layers '(BLANK))
(defvar disabled-layers '())
(defmacro define-layer (layer-name &body body)
  `(progn (push `,',layer-name layers)
	  (defun ',layer-name ()
	    ,@body)))
	    
(define-layer background
    (render-box 0 0 *screen-width* *screen-height* :color *black*)
  (render-box 10 10 (- *screen-width* 10) (- *screen-height* 10) :color +cobalt+))
(define-layer tile-map-bg
    ;;
    )
(define-layer tile-map-fg
    )
(define-layer enemy-sprites
    )
(define-layer player-sprites ;;i.e. Samus and her missiles
    )
(define-layer object-sprites
    )

(defun draw-layers ()
  (if (eq (last layers) 'BLANK)
      (setf layers (reverse layers)))
  (loop for layer in layers
       do (funcall layer)))
