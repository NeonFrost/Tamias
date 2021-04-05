#|
Boiler plate
|#

(defun save-game (file-name things-to-save)
  "Things to save is a quoted list. i.e. '((player-position player) (player-state player) enemy-list)"
  ;;Eventually, it'll be like this: (foramt t "(setf ~S ~A)" thing (eval thing))
  (loop for thing in things-to-save
     do (princ thing) (princ (eval thing))))

(defun load-game (file-name)
  (load file-name))
