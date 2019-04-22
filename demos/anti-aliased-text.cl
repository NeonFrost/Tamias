(define-state aa-text)
(setf state aa-text)

(defvar test-text "This should be anti-aliased text")
(defvar current-alpha 255)
(defvar ca-mode 'dec)

(defun draw-aa-text ()
  (let ((aa-text-x 16)
	(aa-text-y 16))
  (render-string test-text
		 aa-text-x
		 aa-text-y
		 :width (text-width test-text)
		 :height (text-height test-text)
		 :color (list 250 250 250 255))
  (render-string test-text
		 aa-text-x
		 (1- aa-text-y)
		 :width (text-width test-text)
		 :height (text-height test-text)
		 :color (list 250 250 250 100))
  (render-string test-text
		 aa-text-x
		 (1+ aa-text-y)
		 :width (text-width test-text)
		 :height (text-height test-text)
		 :color (list 250 250 250 100))
  (render-string test-text
		 (1+ aa-text-x)
		 aa-text-y
		 :width (text-width test-text)
		 :height (text-height test-text)
		 :color (list 250 250 250 100))
  (render-string test-text
		 (1- aa-text-x)
		 aa-text-y
		 :width (text-width test-text)
		 :height (text-height test-text)
		 :color (list 250 250 250 100))
#|  (if (eq ca-mode 'inc)
      (incf current-alpha 10))
  (if (> current-alpha 235)
      (setf ca-mode 'dec))
  (if (eq ca-mode 'dec)
      (decf current-alpha 10))
  (if (< current-alpha 20)
      (setf ca-mode 'inc))
  |#))
(add-to-state-render draw-aa-text aa-text)
