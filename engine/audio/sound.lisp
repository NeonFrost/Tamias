(defstruct sample
  path
  chunk)

(defun init-sound (sample)
  (setf (sample-chunk `,sample) (sdl2-mixer:load-wav (sample-path sample)))
  )

(defmacro create-sound (var path)
  `(defvar ,var (make-sample :path ,path)))

(defun play-sound (sound channel)
  (let ((sample (sample-chunk sound)))
    (sdl2-mixer:play-channel channel sample 0))
  )
#|
;;;;This v is bad, load-wav will *literally* load the fucking sound, bringing down the lifetime of the hard drive, FIX NEEDED
  (let ((sound (sdl2-mixer:load-wav sound)))
;;;;    (setf sample-track sound)
    (sdl2-mixer:play-channel channel sound 0)
;;;;    (free-sound sound)
    )
  )
|#

(defun free-sound (sound)
  (sdl2-mixer:halt-channel 0)
  (sdl2-mixer:free-chunk sound)
  )
