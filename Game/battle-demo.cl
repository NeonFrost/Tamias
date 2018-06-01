(define-state battle)
(defvar monster nil)
(setf state 'battle)

(defstruct (monster (:include entity
			      (hp 100)
			      (max-hp 100)
			      (defense 3))))
(setf monster (make-monster))

(defun render-hp ()
  (let ((color (if (<= (monster-hp monster) (round (/ (monster-max-hp monster) 4)))
		   +red+
		   (if (<= (monster-hp monster) (round (/ (monster-max-hp monster) 2)))
		       +orange+
		       (if (<= (monster-hp monster) (round (* (/ (monster-max-hp monster) 4) 3)))
			   +yellow+
			   +green+)))))
    (render-box 200
		200
		(monster-hp monster)
		16
		:color color)))
(defun render-monster ()
  (render-box 240
	      232
	      20
	      20
	      :color +pastel-grey+)
  (render-box 230
	      252
	      40
	      50
	      :color +dark-pastel-grey+)
  (render-box 200
	      252
	      30
	      20
	      :color +cobalt+)
  (render-box 270
	      252
	      30
	      20
	      :color +cobalt+)
  (render-box 230
	      302
	      15
	      20
	      :color +orange+)
  (render-box 255
	      302
	      15
	      20
	      :color +orange+)
  )

(defun battle-render ()
  (render-string "Press h to increase hp, press u to decrease hp." 10 5 :color +yellow+)
  (render-hp)
  (render-monster))
(add-to-state-render battle-render battle)

(defun inc-hp ()
  (if (< (monster-hp monster) 100)
      (incf (monster-hp monster) 1)))
(defun dec-hp ()
  (if (> (monster-hp monster) 1)
      (decf (monster-hp monster) 1)))
(add-key :scancode-u battle :down (dec-hp))
(add-key :scancode-h battle :down (inc-hp))
