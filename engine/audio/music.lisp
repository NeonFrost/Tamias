(defstruct track
  path
  stream
  loop-point)

(defmacro define-track (track path &key (loop-point 0.0))
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

#|
A note about the music subsystem
Basically, I want to set it up to where when you change state, it saves the current position of the current track and resumes the track from that point when you return to that state
I.e. The overworld music resumes from where it was when you leave a battle
Or the Town music starts up where it paused when you exit the shop

So, this means that tamias needs to get the position of the current track and save it somewhere. That somewhere is what I'm trying to decide on
|#

(defun loop-track (track)
  "An helper-function for sdl2-mixer's SetMusicPosition in common lisp. Requires ogg format. Uses the \":sdl2-ffi.functions\" package."
  (sdl2-ffi.functions:mix-set-music-position (coerce (track-loop-point track) 'double-float))
  )

(defun start-music (track &key loop-point -volume-state-)
  (let ((loop-point (track-loop-point track)))
    (setf tamias:current-track track)
    (setf (track-stream tamias:current-track) (sdl2-mixer:load-music (track-path tamias:current-track)))
    (sdl2-mixer:play-music (track-stream tamias:current-track) -1)
    (if loop-point
	(sdl2-ffi.functions:mix-hook-music-finished (loop-track tamias:current-track)))
    (setf tamias:volume-state -volume-state-)
    (sdl2-mixer:volume-music tamias:track-volume)))

(defun start-track (track &key loop-point (-volume-state- 'increasing))
  (start-music track :loop-point loop-point :-volume-state- -volume-state-))

(defun pause-music ()
  (sdl2-ffi.functions:mix-pause-music)
  )

(defun stop-music ()
  (sdl2-mixer:halt-music))

(defun resume-music ()
  (sdl2-mixer:volume-music 0)
  (setf tamias:track-volume 0)
  (sdl2-ffi.functions:mix-resume-music)
  (setf tamias:volume-state tamias:default-volume-state)
  )

(defun quit-audio ()
  (sdl2-mixer:halt-music)
  (if tamias:current-track
      (if (track-stream tamias:current-track)
	  (progn (sdl2-mixer:free-music (track-stream tamias:current-track))
		 (setf tamias:current-track nil))))
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit))

(defun switch-track-to (track)
  (setf tamias:new-track track)
  (change-track))

(defun change-track ()
  (sdl2-mixer:halt-music)
  (if (track-stream tamias:current-track)
      (sdl2-mixer:free-music (track-stream tamias:current-track)))
  (setf tamias:current-track (eval tamias:new-track))
  (setf tamias:new-track nil)
  (if tamias:current-track
      (start-music tamias:current-track)
      (setf tamias:volume-state nil)))

(defun test-music ()
  (case tamias:volume-state
    (increasing (if (< tamias:track-volume tamias:max-volume)
		    (progn (incf tamias:track-volume 10)
			   (sdl2-mixer:volume-music tamias:track-volume))
		    (setf tamias:volume-state nil)))
    (decreasing (if (> tamias:track-volume 0)
		    (progn (decf tamias:track-volume 10)
			   (sdl2-mixer:volume-music tamias:track-volume))
		    (setf tamias:volume-state nil)))
    ((changing track-changing) (if (> tamias:track-volume 0)
				   (progn (decf tamias:track-volume 10)
					  (sdl2-mixer:volume-music tamias:track-volume))
				   (progn (setf tamias:volume-state nil)
					  (change-track))))))

(defun lower-volume (delta)
  (decf tamias:max-volume delta)
  (setf tamias:track-volume tamias:max-volume)
  (sdl2-mixer:volume-music tamias:track-volume))

(defun raise-volume (delta)
  (incf tamias:max-volume delta)
  (setf tamias:track-volume tamias:max-volume)
  (sdl2-mixer:volume-music tamias:track-volume))

(defun increase-volume (delta)
  (incf tamias:max-volume delta)
  (setf tamias:track-volume tamias:max-volume)
  (sdl2-mixer:volume-music tamias:track-volume))
