(defpackage :timer
  (:use :cl)
  (:export timers
	   make
	   current
	   end
	   increment
	   increment-timers
	   push-timer
	   remove-timer))
(in-package :timer)
(defvar timers nil)
(defstruct timer
  current
  end
  increment)

(defun make (&key current end increment)
  (make-timer :current current :end end :increment increment))
(defun current (timer)
  (timer-current timer))
(defun end (timer)
  (timer-end timer))
(defun increment (timer)
  (timer-increment timer))

(defun increment-timers ()
  (loop for timer in timer:timers
     do (incf (timer:current timer) (timer:increment timer))
       (if (> (timer:increment timer) 0)
	   (if (> (timer:current) (timer:end))
	       (remove timer (timer:timers)))
	   (if (< (timer:current) (timer:end))
	       (remove timer (timer:timers))))))

(defun push-timer (timer)
  (push timer timer:timers))

(defun remove-timer (timer)
  (setf timer:timers (remove timer timer:timers)))



"A quick thing about timers: they are generalized. So, you can use a timer for a literal timer (i.e. counting down from 60 to 0) 
or you can use it in combination with a GUI (i.e. counting down from 1000 gold to 25 gold)"
