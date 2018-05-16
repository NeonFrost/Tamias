(defvar +track-volume+ 125)
(defvar max-volume 125)
(defvar *current-track* nil)
(defvar *new-track* nil) ;;needs to be a symbol
(defvar default-volume-state 'increasing)
(defstruct track
  path
  stream
  loop-point)

(defmacro define-track (track path &key (loop-point 0))
  `(defvar ,track (make-track :path ,path :loop-point ,loop-point)))

#|
int music_internal_position(double position)
{
    if (music_playing->interface->Seek) {
        return music_playing->interface->Seek(music_playing->context, position);
    }
    return -1;
}
|#

(defun loop-track (track)
  "An helper-function for sdl2-mixer's SetMusicPosition in common lisp. Requires ogg format. Uses the \":sdl2-ffi.functions\" package."
  (sdl2-ffi.functions:mix-set-music-position (coerce (track-loop-point track) 'double-float))
  )

(defun start-music (track &key loop-point (-volume-state- 'increasing))
  (setf *current-track* track)
  (setf (track-stream *current-track*) (sdl2-mixer:load-music (track-path *current-track*)))
  (sdl2-mixer:play-music (track-stream *current-track*) -1)
  (if loop-point
      (setf (track-loop-point *current-track*) loop-point))
  (setf volume-state -volume-state-)
  (sdl2-mixer:volume-music +track-volume+)
  )

(defun pause-music ()
  (sdl2-ffi.functions:mix-pause-music)
  )

(defun stop-music ()
  (sdl2-mixer:halt-music)
  )

(defun resume-music ()
  (sdl2-mixer:volume-music 0)
  (setf +track-volume+ 0)
  (sdl2-ffi.functions:mix-resume-music)
  (setf volume-state default-volume-state)
  )

(defun quit-audio ()
  (sdl2-mixer:halt-music)
  (if *current-track*
      (if (track-stream *current-track*)
	  (progn (sdl2-mixer:free-music (track-stream *current-track*))
		 (setf *current-track* nil))))
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit)
  )

(defun switch-track-to (track)
  (setf *new-track* track)
  (change-track))

(defun change-track ()
  (sdl2-mixer:halt-music)
  (if (track-stream *current-track*)
      (sdl2-mixer:free-music (track-stream *current-track*)))
  (setf *current-track* (eval *new-track*))
  (setf *new-track* nil)
  (if *current-track*
      (start-music *current-track*)
      (setf volume-state nil)))

(defun test-music ()
  (case volume-state
    (increasing (if (< +track-volume+ max-volume)
		    (progn (incf +track-volume+ 10)
			   (sdl2-mixer:volume-music +track-volume+))
		    (setf volume-state nil)))
    (decreasing (if (> +track-volume+ 0)
		    (progn (decf +track-volume+ 10)
			   (sdl2-mixer:volume-music +track-volume+))
		    (setf volume-state nil)))
    ((changing track-changing) (if (> +track-volume+ 0)
			(progn (decf +track-volume+ 10)
			       (sdl2-mixer:volume-music +track-volume+))
			(progn (setf volume-state nil)
			       (change-track))))))

(defun lower-volume (delta)
  (decf max-volume delta)
  (setf +track-volume+ max-volume)
  (sdl2-mixer:volume-music +track-volume+))

(defun raise-volume (delta)
  (incf max-volume delta)
  (setf +track-volume+ max-volume)
  (sdl2-mixer:volume-music +track-volume+))

(defun increase-volume (delta)
  (incf max-volume delta)
  (setf +track-volume+ max-volume)
  (sdl2-mixer:volume-music +track-volume+))
