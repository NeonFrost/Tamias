(defvar *tamias-stream* *standard-output*)
(defvar *default-tamias-stream* *standard-output*)
(defun t-princ (&rest strs)
  (loop for str in strs
     do (princ str *tamias-stream*)))

(defun t-print (&rest strs)
  (loop for str in strs
     do (print str *tamias-stream*)))

(defun t-string (&rest strs)
  (setf *tamias-stream* (make-string-output-stream))
  (loop for str in strs
     do (princ str *tamias-stream*))
  (princ (get-output-stream-string *tamias-stream*))
  (close *tamias-stream*)
  (setf *tamias-stream* *default-tamias-stream*))

(defun make-t-string (&rest strs)
  (let ((tmp-stream nil))
    (setf *tamias-stream* (make-string-output-stream))
    (loop for t-str in strs
       do (princ t-str *tamias-stream*))
    (setf tmp-stream (get-output-stream-string *tamias-stream*))
    (close *tamias-stream*)
    (setf *tamias-stream* *default-tamias-stream*)
    (finish-output t)
    (finish-output nil)
    (princ tmp-stream)))
    

;;(make-t-string str "xyz" 'z 123)
