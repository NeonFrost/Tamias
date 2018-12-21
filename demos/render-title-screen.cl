(defvar game-info "")
(defvar title-menus nil)
(define-menu title-menu title-menus
  0 0
  *screen-width* *screen-height*
  +dark-pastel-grey+ +black+)
(define-screen title-screen title-menus)

(defun render-title-screen ()
  (title-screen)
  (let ((title-buffer (create-text-buffer title-name
					  :width (* (length title-name) (car character-size))
					  :height (cadr character-size)
					  :to-texture t
					  :buffer-source 'text))
	(title-options-buffer (create-text-buffer (start-string "  Start  "
								" Options "
								"Exit Game")
						  :width (* (length "Exit Game") (car character-size) 2)
						  :height (* (cadr character-size) 3)
						  :to-texture t
						  :buffer-source 'text))
	(options-buffer (create-text-buffer (start-string "      Volume     "
							  (combine-strings "    Resolution   " (write-to-string *screen-width*) " X " (write-to-string *screen-height*))
							  "Exit to Main Menu")
					    :width (* 30 (car character-size))
					    :height (* (cadr character-size) 3)
					    :to-texture t
					    :buffer-source 'text)))
    (case sub-state
      (top (tex-blit title-buffer
		     :dest (sdl2:make-rect (round (- (/ (menu-width title-menu) 2) (* (car character-size) (length title-name))))
					   (menu-y title-menu)
					   (round (/ (menu-width title-menu) 8))
					   (round (/ (menu-height title-menu) 8))))
	   (render-box (round (- (/ (menu-width title-menu) 2) (/ (menu-width title-menu) 8)))
		       (+ (round (+ (menu-y title-menu) (/ (menu-height title-menu) 2))) (* *selection-row* (cadr character-size) 2))
		       (round (/ (menu-width title-menu) 4))
		       (* (cadr character-size) 2)
		       :color +dark-pastel-grey+)
	   (tex-blit title-options-buffer
		     :dest (sdl2:make-rect (round (- (/ (menu-width title-menu) 2) (/ (* (car character-size) (length "Exit Game")) 2)))
					   (round (+ (menu-y title-menu) (/ (menu-height title-menu) 2)))
					   (round (/ (menu-width title-menu) 4))
					   (* (cadr character-size) 6))))
      (options (render-box (round (- (/ (menu-width title-menu) 2) (/ (menu-width title-menu) 8)))
			   (+ (round (+ (menu-y title-menu) (- (/ (menu-height title-menu) 2) (/ (menu-height title-menu) 32)))) (* *selection-row* (cadr character-size) 2))
			   (round (*  (/ (menu-width title-menu) 8) 3))
			   (* (cadr character-size) 2)
			   :color +dark-pastel-grey+)
	       (tex-blit options-buffer
			 :dest (create-rectangle (list (round (- (/ (menu-width title-menu) 2) (/ (menu-width title-menu) 8)))
						       (round (+ (menu-y title-menu) (- (/ (menu-height title-menu) 2) (/ (menu-height title-menu) 32))))
						       (round (*  (/ (menu-width title-menu) 8) 3))
						       (* (cadr character-size) 6))))
	       ))
    (if (> (length current-text-context) 0)
	(render-string current-text-context 0 0 :width 256 :height 128 :color +white+))
    (reset-text-buffer title-buffer)
    (reset-text-buffer title-options-buffer)
    (reset-text-buffer options-buffer)))
(add-to-state-render render-title-screen title)