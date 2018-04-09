(defstruct sample
  path)

(defun play-sound (sound channel)
  (let ((sound (sdl2-mixer:load-wav sound)))
;;;;    (setf sample-track sound)
    (sdl2-mixer:play-channel channel sound 0)
;;;;    (free-sound sound)
    )
  )

(defun free-sound (sound)
  (sdl2-mixer:halt-channel 0)
  (sdl2-mixer:free-chunk sound)
  )
