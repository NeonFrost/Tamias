(define-state gui-tester)
(set-state gui-tester)

(defvar rend-text nil)
(state.init-ui 'gui-tester 'top)
(def-render ('gui-tester 'top)
  (render.ui-elements gui-tester)
  (render.cursor)
      ;;(render:render-string "Hey Dad, it works!" 232 200 :width 300 :height 16 :string-width (* (length "Hey Dad, it works!") 16) :string-height 16 :anti-alias t))
  )

(add-state-ui-element 'gui-tester 'top (make-ui-button :x 200 :x-equation 200 :y 200 :y-equation 200 :action `(setf rend-text (not rend-text)) :color (list 0 90 0 255)))
(add-state-ui-element 'gui-tester 'top (make-ui-label :x 300 :x-equation 300 :y 300 :y-equation 300 :width 70 :width-equation 70 :height 134 :height-equation 134 :label "Guys, It Works! This splits text and wraps it!" :hidden `,'`,rend-text))
(add-state-ui-element 'gui-tester 'top (make-ui-text :x 50 :x-equation 50 :y 12 :y-equation 12 :width 400 :width-equation 400 :height 24 :height-equation 24))

(add-mouse :button-left ('gui-tester 'top :down nil)
  (tamias.gui.click))
(add-mouse nil ('gui-tester 'top nil t)
  (tamias.gui.mouse-move)
  )

