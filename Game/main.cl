(define-state gui-tester)
(set-state gui-tester)

(defvar rend-text nil)
(state.init-ui 'gui-tester 'top)
(def-render ('gui-tester 'top)
    (render.ui-elements gui-tester)
      ;;(render:render-string "Hey Dad, it works!" 232 200 :width 300 :height 16 :string-width (* (length "Hey Dad, it works!") 16) :string-height 16 :anti-alias t))
  )
(def-render ('gui-tester t)
  (render.menu-bar 'gui-tester 'top))

(add-state-ui-element 'gui-tester 'top (make-ui-button :x 200 :x-equation 200 :y 200 :y-equation 200 :action `(setf rend-text (not rend-text))))
(add-state-ui-element 'gui-tester 'top (make-ui-label :x 300 :x-equation 300 :y 300 :y-equation 300 :width 272 :width-equation 272 :height 16 :height-equation 16 :label "It Works" :hidden `,'`,rend-text))

(add-mouse :button-left ('gui-tester 'top :down nil)
  (tamias.gui.click))
(add-mouse nil ('gui-tester 'top nil t)
;  (setf rend-text (not rend-text))
  (tamias.gui.mouse-move)
  )

