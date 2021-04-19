(defstruct tamias-texture
  file
  id)
(defvar tamias-textures (make-hash-table))

(defun free-tamias-texture (tam-tex)
  ;;free the gl:texture id from GPU memory
  (setf (tamias-texture-id tam-text) nil))
