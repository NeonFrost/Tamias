(define-state stock-simulator)
(set-state stock-simulator)

(defvar stock-timer 0)
(defvar max-time 20)
(state.init-ui 'stock-simulator 'top)
(def-render ('stock-simulator 'top)
    (render.ui-elements stock-simulator)
  (render.cursor))

(def-logic ('stock-simulator 'top)
    (if (eq stock-timer 0)
	(update-stocks))
    (incf stock-timer)
  (if (>= stock-timer max-time)
      (setf stock-timer 0)))

(add-state-ui-element 'stock-simulator 'top
		      (make-ui-label :x 16 :x-equation 16 :y 16 :y-equation 16
				     :width (* 5 (car tamias.string:character-size)) :width-equation (* 5 (car tamias.string:character-size))
				     :height (+ (cadr tamias.string:character-size) 2) :height-equation (+ (cadr tamias.string:character-size) 2)
				     :label '(stock-ticker (nth 0 stocks))))
(add-state-ui-element 'stock-simulator 'top
		      (make-ui-label :x (+ 16 (* 5 (car tamias.string:character-size)) 4)
				     :x-equation (+ 16 (* 5 (car tamias.string:character-size)) 4)
				     :y 16 :y-equation 16
				     :width (* 10 (car tamias.string:character-size))
				     :width-equation (* 10 (car tamias.string:character-size))
				     :height (+ (cadr tamias.string:character-size) 2)
				     :height-equation (+ (cadr tamias.string:character-size) 2)
				     :label '(write-to-string (stock-price (nth 0 stocks)))))
(add-state-ui-element 'stock-simulator 'top
		       (make-ui-label :x (+ 16 (* 5 (car tamias.string:character-size)) 4
					    (* 10 (car tamias.string:character-size)))
				      :x-equation (+ 16 (* 5 (car tamias.string:character-size)) 4
						     (* 10 (car tamias.string:character-size)))
				      :y 16
				      :y-equation 16
				      :width (* 6 16)
				      :width-equation (* 6 16)
				      :height (+ (cadr tamias.string:character-size) 2)
				      :height-equation (+ (cadr tamias.string:character-size) 2)
				      :label '(tamias.string:combine-strings (write-to-string (stock-change (nth 0 stocks))) "%")))

(add-state-ui-element 'stock-simulator 'top
		      (make-ui-label :x 16 :x-equation 16 :y 36 :y-equation 36
				     :width (* 5 (car tamias.string:character-size)) :width-equation (* 5 (car tamias.string:character-size))
				     :height (+ (cadr tamias.string:character-size) 2) :height-equation (+ (cadr tamias.string:character-size) 2)
				     :label '(stock-ticker (nth 1 stocks))))
(add-state-ui-element 'stock-simulator 'top
		      (make-ui-label :x (+ 16 (* 5 (car tamias.string:character-size)) 4)
				     :x-equation (+ 16 (* 5 (car tamias.string:character-size)) 4)
				     :y 36 :y-equation 36
				     :width (* 10 (car tamias.string:character-size))
				     :width-equation (* 10 (car tamias.string:character-size))
				     :height (+ (cadr tamias.string:character-size) 2)
				     :height-equation (+ (cadr tamias.string:character-size) 2)
				     :label '(write-to-string (stock-price (nth 1 stocks)))))
(add-state-ui-element 'stock-simulator 'top
		       (make-ui-label :x (+ 16 (* 5 (car tamias.string:character-size)) 4
					    (* 10 (car tamias.string:character-size)))
				      :x-equation (+ 16 (* 5 (car tamias.string:character-size)) 4
						     (* 10 (car tamias.string:character-size)))
				      :y 36
				      :y-equation 36
				      :width (* 6 16)
				      :width-equation (* 6 16)
				      :height (+ (cadr tamias.string:character-size) 2)
				      :height-equation (+ (cadr tamias.string:character-size) 2)
				      :label '(tamias.string:combine-strings (write-to-string (stock-change (nth 1 stocks))) "%")))

(add-state-ui-element 'stock-simulator 'top
		      (make-ui-label :x 16 :x-equation 16 :y 56 :y-equation 56
				     :width (* 5 (car tamias.string:character-size)) :width-equation (* 5 (car tamias.string:character-size))
				     :height (+ (cadr tamias.string:character-size) 2) :height-equation (+ (cadr tamias.string:character-size) 2)
				     :label '(stock-ticker (nth 2 stocks))))
(add-state-ui-element 'stock-simulator 'top
		      (make-ui-label :x (+ 16 (* 5 (car tamias.string:character-size)) 4)
				     :x-equation (+ 16 (* 5 (car tamias.string:character-size)) 4)
				     :y 56 :y-equation 56
				     :width (* 10 (car tamias.string:character-size))
				     :width-equation (* 10 (car tamias.string:character-size))
				     :height (+ (cadr tamias.string:character-size) 2)
				     :height-equation (+ (cadr tamias.string:character-size) 2)
				     :label '(write-to-string (stock-price (nth 2 stocks)))))
(add-state-ui-element 'stock-simulator 'top
		       (make-ui-label :x (+ 16 (* 5 (car tamias.string:character-size)) 4
					    (* 10 (car tamias.string:character-size)))
				      :x-equation (+ 16 (* 5 (car tamias.string:character-size)) 4
						     (* 10 (car tamias.string:character-size)))
				      :y 56
				      :y-equation 56
				      :width (* 6 16)
				      :width-equation (* 6 16)
				      :height (+ (cadr tamias.string:character-size) 2)
				      :height-equation (+ (cadr tamias.string:character-size) 2)
				      :label '(tamias.string:combine-strings (write-to-string (stock-change (nth 2 stocks))) "%")))
