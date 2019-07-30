(defpackage :tamias
  (:use :cl)
  (:export key-pressed
	   screen-surface
	   default-window
	   screen-width
	   screen-height
	   game-title
	   state
	   sub-state
	   changing-state
	   renderer
	   accumulator
	   selection
	   font
	   font-color
	   max-characters
	   buffers
	   cell-accumulator
	   volume-state
	   objects
	   render-clear-color
	   resolution
	   update-time
	   fps
	   resolution-list))
(in-package :tamias)
(defvar key-pressed nil)
(defvar screen-surface nil)
(defvar default-window nil)
(defvar screen-width 0)
(defvar screen-height 0)
(defvar game-title "Pong")
(defvar state nil) ;;specifically an empty state, does nothing
(defvar sub-state 'top)
(defvar changing-state nil) ;;setf changing-state to 'state-to-change-to
(defvar renderer nil)
(defvar accumulator 0)
(defvar selection 0)
(defvar font nil)
(defvar font-color '(255 255 255 0))
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
