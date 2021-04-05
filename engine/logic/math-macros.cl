(defmacro a<n<c (a n c)
  `(and (> ,n ,a)
	(< ,n ,c)))

(defmacro a<=n<=c (a n c)
  `(and (>= ,n ,a)
	(<= ,n ,c)))

(defmacro a>n>c (a n c)
  `(and (< ,n ,a)
	(> ,n ,c)))

(defmacro a>=n>=c (a n c)
  `(and (<= ,n ,a)
	(>= ,n ,c)))
