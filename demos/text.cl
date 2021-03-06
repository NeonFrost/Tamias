(define-state text-example)
(setf state 'text-example)

(defvar current-color-state 0)
(defstruct color-text
  (string "O: Colored and rotating Text!! :D")
  (color (list 230 0 0 255))
  (rotate 0)
  (rotate-inc 3)
  (scale 1.0)
  (scale-incrementor 0.1)
  (state '(r rg g gb b rb))
  )
(defvar color-text (make-color-text))

(defun modify-text ()
  (case (nth current-color-state (color-text-state color-text))

    (r (if (< (car (color-text-color color-text)) 230)
	   (incf (nth 0 (color-text-color color-text)) 30))

       (if (>= (caddr (color-text-color color-text)) 30)
	   (decf (nth 2 (color-text-color color-text)) 30))

       (if (and (>= (car (color-text-color color-text)) 230)
		(<= (cadr (color-text-color color-text)) 30))
	   (incf current-color-state)))
    
    (rg (incf (nth 1 (color-text-color color-text)) 30)
	(if (>= (cadr (color-text-color color-text)) 230)
	    (incf current-color-state)))

    (g (decf (nth 0 (color-text-color color-text)) 30)
       (if (<= (car (color-text-color color-text)) 30)
	   (incf current-color-state)))

    (gb (incf (nth 2 (color-text-color color-text)) 30)
	(if (>= (caddr (color-text-color color-text)) 230)
	   (incf current-color-state)))

    (b (decf (nth 1 (color-text-color color-text)) 30)
       (if (<= (cadr (color-text-color color-text)) 30)
	   (incf current-color-state)))

    (rb (incf (nth 0 (color-text-color color-text)) 30)
	(if (>= (car (color-text-color color-text)) 230)
	    (setf current-color-state 0))))
  
  (if (>= (color-text-scale color-text) 3.0)
      (setf (color-text-scale-incrementor color-text) -0.1)

      (if (<= (color-text-scale color-text) 1.0)
	  (setf (color-text-scale-incrementor color-text) 0.1)))
  
  (incf (color-text-scale color-text) (color-text-scale-incrementor color-text))

  (if (>= (color-text-rotate color-text) 45)
      (setf (color-text-rotate-inc color-text) -3)
      (if (<= (color-text-rotate color-text) -45)
	  (setf (color-text-rotate-inc color-text) 3)))

  (incf (color-text-rotate color-text) (color-text-rotate-inc color-text))
  )

(defun process-text ()
  (render-box 0 0 *screen-width* *screen-height* :color +black+)
  (render-text (color-text-string color-text) 700 350 ::width (text-width (color-text-string color-text)) :height (text-height (color-text-string color-text))
	       :color (color-text-color color-text) :rotate (color-text-rotate color-text) :scale (color-text-scale color-text))
  (modify-text))
(add-to-state-render process-text text-example)
#|
(defun draw-secondary-text ()
  (render-string "Fuuuuu" 100 100 :width (text-width "Fuuuuu") :height (text-height "Fuuuuu") :dest-width 400 :dest-height 400 :color +white+))
(add-to-state-render draw-secondary-text text-example)
(defun draw-font-tmp ()
  (let ((font-sheet (sdl2:create-texture-from-surface renderer +font-sheet+)))
    (tex-blit font-sheet :dest (create-rectangle (list 0 0 *screen-width* *screen-height*)) :color (color-text-color color-text) :angle (color-text-rotate color-text))))
(add-to-state-render draw-font-tmp text-example)
|#
