(defstruct sample
  path
  chunk)
(defvar *tamias-sounds* nil)

(defun init-sound (sample)
  (setf (sample-chunk sample) (sdl2-mixer:load-wav (sample-path sample)))
  )
(defun init-sounds ()
  (loop for sound in *tamias-sounds*
     do (init-sound (eval sound))))

(defmacro define-sound (var path)
  `(progn (defvar ,var (make-sample :path ,path))
	  (push ,'var *tamias-sounds*)))

(defun play-sound (sound channel)
  (let ((sample (sample-chunk sound)))
    (sdl2-mixer:play-channel channel sample 0))
  )
#|
  (let ((sound (sdl2-mixer:load-wav sound)))
;;;;    (setf sample-track sound)
    (sdl2-mixer:play-channel channel sound 0)
;;;;    (free-sound sound)
    )
  )
|#

(defun free-sound (sound)
  (sdl2-mixer:halt-channel 0)
  (sdl2-mixer:free-chunk (sample-chunk sound))
  (setf (sample-chunk sound) nil))

(defun free-sounds ()
  (sdl2-mixer:halt-channel 0)
  (loop for sound in *tamias-sounds*
     do (sdl2-mixer:free-chunk (sample-chunk (eval sound)))))
