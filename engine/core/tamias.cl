(defpackage :tamias
  (:use :cl)
  (:export *mouse-x*
	   *mouse-y*
	   *mouse-velocity-x*
	   *mouse-velocity-y*
	   *cursor-size*
	   track-volume
	   max-volume
	   current-track
	   new-track
	   default-volume-state
	   key-pressed
	   screen-surface
	   default-window
	   screen-width
	   screen-height
	   title
	   state
	   sub-state
	   changing-state
	   renderer
	   accumulator
	   selection
	   font
	   font-color
	   ttf-font
	   ttf-font-size
	   ttf-font-path
	   max-characters
	   buffers
	   cell-accumulator
	   volume-state
	   objects
	   render-clear-color
	   resolution
	   update-time
	   fps
	   resolution-list
	   messages
	   messages-counter
	   messages-color
	   enable-ttf-font
	   using-gui
	   console-messages
	   console.add-message
	   console.show
	   console.hide
	   console.toggle))
(in-package :tamias)

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *mouse-velocity-x* 0)
(defvar *mouse-velocity-y* 0)
(defparameter *cursor-size* 5)

(defvar track-volume 125)
(defvar max-volume 125)
(defvar current-track nil)
(defvar new-track nil) ;;needs to be a symbol
(defvar default-volume-state 'increasing)

(defvar messages nil)
(defvar messages-counter 0)
(defvar messages-color '(255 255 0 255))

(defvar key-pressed nil)
(defvar screen-surface nil)
(defvar default-window nil)
(defvar screen-width 0)
(defvar screen-height 0)
(defvar title "Pong")

(defvar state nil) ;;specifically an empty state, does nothing
(defvar sub-state 'top)
(defvar changing-state nil) ;;setf changing-state to 'state-to-change-to (i.e. main-menu)
(defvar renderer nil)
(defvar accumulator 0)
(defvar selection 0)

(defvar font nil)
(defvar font-color '(255 255 255 0))
(defvar ttf-font nil)
(defvar ttf-font-size 12)
(defvar ttf-font-path "fonts/linux_libertine/LinLibertine_R.ttf")

(defvar max-characters '(40 40))
(defvar buffers nil)
(defvar cell-accumulator 0)
(defvar volume-state nil)
(defvar objects nil)
(defvar render-clear-color '(0 0 0))
(defvar resolution 4)
(defvar update-time 10)
(defvar fps 30)
(defvar resolution-list '((800 600)
			  (960 720)
			  (1024 768)
			  (1280 720)
			  (1360 760)
			  (1280 1024)
			  (1440 900)
			  (1680 1050)
			  (1280 960)
			  (1440 1080)
			  (1920 1080)))
(setf screen-width (car (nth resolution resolution-list)))
(setf screen-height (cadr (nth resolution resolution-list)))
(defun enable-ttf-font ()
  (if (not ttf-font)
      (setf ttf-font t)
      ;;tamias message: TTF font is already enabled
      ))

(defvar console-messages (list "console started"))
(defun console.add-message (message)
  (setf console-messages (append console-messages (list message))))
(defvar console.show t)
(defun console.toggle ()
  (setf console.show (not console.show)))
(defun console.show ()
  (setf console.show t))
(defun console.hide ()
  (setf console.show nil))

(defvar using-gui nil)


