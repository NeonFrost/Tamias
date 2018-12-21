(define-state battle)
(defvar monster nil)
(setf state 'battle)

(defstruct (monster (:include entity)))
(setf monster (make-monster))
(setf (entity-hp monster) 100
      (entity-max-hp monster) 100
      (entity-defense monster) 3)

(defun render-hp ()
  (let ((color (if (<= (entity-hp monster) (round (/ (entity-max-hp monster) 4)))
		   +red+
		   (if (<= (entity-hp monster) (round (/ (entity-max-hp monster) 2)))
		       +orange+
		       (if (<= (entity-hp monster) (round (* (/ (entity-max-hp monster) 4) 3)))
			   +yellow+
			   +green+)))))
    (render-box 200
		200
		(entity-hp monster)
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
  (if (< (entity-hp monster) 100)
      (incf (entity-hp monster) 1)))
(defun dec-hp ()
  (if (> (entity-hp monster) 1)
      (decf (entity-hp monster) 1)))
(add-key :scancode-u battle :down (dec-hp))
(add-key :scancode-h battle :down (inc-hp))
