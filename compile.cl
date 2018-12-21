(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :tamias)

(defun load-project (project)
  (asdf:load-system project))

(defun load-demos ()
  (load-project :demos))

(defmacro load-demo (demo)
  `(let ((demo-name (string-downcase (write-to-string ',demo))))
     (load (concatenate 'string "demos/" demo-name ".cl"))))
