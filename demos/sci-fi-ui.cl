(define-state sci-fi)
(setf state 'sci-fi)

(defvar sf-width 192)
(defvar sf-height 32)
(defun render-ui-element (x y str)
  (render-box x y sf-width sf-height :color (list 0 34 55 255))
  (render-box (+ x 2) (+ y 2) (- sf-width 4) (- sf-height 4) :color (list 0 19 32 255))
  (render-box (+ x 4) (+ y 4) (- sf-width 8) (- sf-height 8) :color (list 0 73 118 255))
  (render-box (+ x 8) (+ y 8) (round (/ sf-height 2)) (round (/ sf-height 2)) :color (list 0 33 54 255))
  (render-box (+ x 10) (+ y 10) (round (* (/ sf-height 16) 6)) (round (* (/ sf-height 16) 6)) :color (list 0 103 167 255))
  (render-box (+ x 36) (+ y 8) (- sf-width 50) (- sf-height 16) :color (list 106 170 210 255))
  (render-string str (+ x 36) (+ y 8) :color +black+)
  )

(defun render-sf-selector (x y) ;selection * box height + 1/2 box height
  (render-box (+ x 12) (+ y 12) 8 8 :color (list 171 223 255 255))
  )

(defvar selector 0)

(defun render-ui ()
  (render-ui-element 32 16 "Attack")
  (render-ui-element 32 (+ 16 (* 16 2)) "Tech")
  (render-ui-element 32 (+ 16 (* 16 4)) "Defend")
  (render-ui-element 32 (+ 16 (* 16 6)) "Item")
  (render-ui-element 32 (+ 16 (* 16 8)) "Switch")
  (render-ui-element 32 (+ 16 (* 16 10)) "Something")
  (render-ui-element 32 (+ 16 (* 16 12)) "Run")
  (render-sf-selector 32 (+ (* selector 32) 16))
  )
(add-to-state-render render-ui sci-fi)

(add-key :scancode-down sci-fi :down (if (< selector 6)
					 (incf selector 1)
					 (setf selector 0)))
(add-key :scancode-up sci-fi :down (if (> selector 0)
				       (decf selector 1)
				       (setf selector 6)))

