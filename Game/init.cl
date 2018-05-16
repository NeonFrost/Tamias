(define-state title)
(define-state level)
(define-state game-over)

(defvar equipment (make-state :transition nil))
(defvar paused (make-state :transition nil))
(start-input equipment)
(start-input paused)

(defstruct (grapple-surface (:include entity))
  (grapple-able t)
  ;;tile-map
  )
(defstruct (player (:include entity
			     (x (round (/ *screen-width* 2)))
			     (y (round (* (/ *screen-height* 8) 7)))
			     (width 8)
			     (height 16)
			     (symbol-color +green+)))
  grappling
  grapple-texture 
  (grapple-position (make-vector-3d))
  (grapple-vector (make-vector-3d)))

(defvar +grapple-animation+ nil)
(defvar +grapple-points+ 0)
(defvar +current-grapple-point+ nil)
(defvar +pulling-player+ nil)
(defvar entities nil)
(defvar surfaces nil)
(defvar game-floor (make-entity))
(push 'game-floor surfaces)
(defvar grapple-wall-1 (make-grapple-surface))
(push 'grapple-wall-1 surfaces)
(defvar grapple-wall-2 (make-grapple-surface))
(push 'grapple-wall-2 surfaces)
(defvar player (make-player))
(push 'player entities)

(define-track main-menu-track "Game/main.ogg")
(define-track level-track "Game/level.ogg")

(defun exit-to-main-menu ()
  (setf state 'title)
  (setf selection 0)
  (sdl2-mixer:halt-music)
  (switch-track-to main-menu-track)
  )

