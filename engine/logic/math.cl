(defun right-triangle (a b)
  (sqrt (+ (* a a) (* b b))))

(defun rad->degrees (r &key (equation 'sin))
  (case equation
    (sin (/ 180 (sin r)))
    (cos (/ 180 (cos r)))
    (tan (/ 180 (tan r)))))

(defun find-angle (a b &key (equation 'sin))
  (case equation
    (sin (rad->degrees (/ a (right-triangle a b)) :equation equation))
    (cos (rad->degrees (/ b (right-triangle a b)) :equation equation))
    (tan (rad->degrees (/ a b) :equation equation))
    ))
