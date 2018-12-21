(define-state animation)
(setf state animation)

(defvar nika-sheet (make-sprite-sheet :file "demos/Nika.png"))

(defvar nika-current-cell 0)
(defvar dir 1)
(defvar cell-timer 0)
(setf tamias-renderer-clear-color '(100 100 100 255))

(defun draw-nika ()
  (if (not (sprite-sheet-cells nika-sheet))
      (load-sheet nika-sheet '(64 64)))
  (draw-cell nika-sheet nika-current-cell
	     200 200
	     :width 256 :height 256))
(add-to-state-render draw-nika animation)

(defun increase-cell ()
  (if (not (sprite-sheet-cells nika-sheet))
      (load-sheet nika-sheet '(64 64)))
  (incf cell-timer 5)
  (if (> cell-timer 5)
      (progn (incf nika-current-cell 1)
	     (setf cell-timer 0)))
  (if (>= nika-current-cell (* dir 8))
      (setf nika-current-cell (- (* dir 8) 8))))
(add-loop-function increase-cell animation 'top)

(add-key :scancode-down animation :down (setf dir 1
					      nika-current-cell (- (* dir 8) 8)))
(add-key :scancode-up animation :down (setf dir 2
					    nika-current-cell (- (* dir 8) 8)))
(add-key :scancode-right animation :down (setf dir 3
					       nika-current-cell (- (* dir 8) 8)))
(add-key :scancode-left animation :down (setf dir 4
					      nika-current-cell (- (* dir 8) 8)))

