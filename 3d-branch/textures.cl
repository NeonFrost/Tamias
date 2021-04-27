(defstruct tamias-texture
  file
  id)
(defvar tamias-textures (make-hash-table))

(defun free-tamias-texture (tam-tex)
  ;;free the gl:texture id from GPU memory
  (gl:delete-texture (tamias-texture-id tam-tex))
  (setf (tamias-texture-id tam-text) nil))

(defun load-tamias-texture (tam-tex)
  (setf (tamias-texture-id tam-tex) (load-texture (tamias-texture-file tam-tex))))
  
