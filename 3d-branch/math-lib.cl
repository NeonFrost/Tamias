#|

Time for some fun!

So, how to do this:
use calculate-transformation to get the transformation matrix per 'frame'
now, get the timing of that point in animation (i.e. 28.0, but KF[0] is 14.0 and KF[1] is 30.0)
normalize the time to '1', so [tA, tB] = [KF[0] - KF[0] + 1, KF[1] - KF[0] + 1], next normalize the timing to '1', so timing - tB - 1
next, multiply the transformation-per-frame matrix by the normalized timing, and voila, you have the transformation at t.

So:
trans-mat = (calc-trans bone[0 0] bone[1 0] kf[0] kf[1])
-> #2A((0.0 0.0 1.2e-12 0) (0.0 5.55555e-12 0.0 0.0) (2.1e-12 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
normalize: [1, kf[1] - kf[0] + 1] = [1.0, 5.0], current-time = 8.0, normalized ct = 3.0
ct * trans-mat
->#2A((0.0 0.0 3.6e-12 0) (0.0 1.555555e-11 0.0 0.0) (6.3e-12 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))

My recommendation is to get the transformation mats of the bones into an array and send this array of mats to the GPU
Why calc on the CPU first? It's 32 bones once per frame vs 32 bones once per vertex. sure on 4 verts it isn't a big deal, but most models with armatures don't have just 4 verts
The timing should calced on the CPU, along with the matrices of that frame.
Once those calculations are done (the ones that would take up resources on the GPU) then you send them to the GPU, so that you aren't redoing the /exact same/ calculations
every vertex.
Also, the calculated transform should NOT be saved. You can, but I wouldn't recommend it.


I was misunderstanding some things about bone animations:
So, inverse bind pose is (1 / bind pose) not (-1 * bind pose) Me am dumb dumb
I am very dumb. the inverse of a matrix is not (1/mat[i j]). It's much more complicated than that. Gettin' the determinant and fuckin bullshit
Get the Determinant of a 4x4 matrix, then get the determinants of all the 2x2 matrices of the 4x4 matrix. 
This 4x4 matrix of the determinants of the 2x2 mats is called the adjugate matrix
Now, you take the adjugate matrix and divide each element by the determinant of the 4x4 matrix
Now you have the inverse of the original 4x4 matrix




So: Frame 4 animation:
get the transformation at Frame 4 (matA keyframe0, matB keyframe1) -> kf0 = 1 kf1= 9 -> matA(1.0) matB(9.0)
matC=(8.0) next matc/num-frames, so matD=(1.0), next matD*(frame_num-kf0) = (3.0)


|#

(defmacro make-mat3 ()
  `(make-array '(3 3) :initial-contents '((1.0 0.0 0.0)
					  (0.0 1.0 0.0)
					  (0.0 0.0 1.0))))
(defmacro make-mat4 ()
  `(make-array '(4 4) :initial-contents '((1.0 0.0 0.0 0.0)
					  (0.0 1.0 0.0 0.0)
					  (0.0 0.0 1.0 0.0)
					  (0.0 0.0 0.0 1.0))))

(defun calculate-frame-transformation (mat-A mat-B timing-A timing-B)
  "This calculates a matrix along a linear path of a keyframed bone animation. I.e. mat-A mat-B are bone-mat[0] and bone-mat[1] where 0 and 1 are the keyframes.
timing-A and timing-B are the times of the keyframes, for example 7.0 and 28.0."
  (let ((res-mat (make-mat4))
	(timing 0.0))
    (setf timing (- (- timing-B timing-A) 1.0))
    (loop for i below 4
	  do (loop for j below 4
		   do (setf (aref res-mat i j) (/ (- (aref mat-B i j) (aref mat-A i j)) timing)))) ;;so mat-B - mat-A would get us the total transformation between the 2 keyframes. Div by timing gives us, given linear interpolation, the 'rate of change' matrix.
    res-mat))


#|
(defun calculate-transformation (matA matB timing) ;;the timing is to multiply the frame-transformation by
)
|#
(defun mat2-det (mat-A)
  (- (* (aref mat-A 0 0)
	(aref mat-A 1 1))
     (* (aref mat-A 0 1)
	(aref mat-A 1 0))))

(defun mat3-det (mat-A)
  (let ((det 0.0))
    (incf det (* (aref mat-A 0 0)
		 (- (* (aref mat-A 1 1)
		       (aref mat-A 2 2))
		    (* (aref mat-A 1 2)
		       (aref mat-A 2 1)))))
    (incf det (* -1
		 (aref mat-A 0 1)
		 (- (* (aref mat-A 1 0)
		       (aref mat-A 2 2))
		    (* (aref mat-A 1 2)
		       (aref mat-A 2 0)))))
    (incf det (* (aref mat-A 0 2)
		 (- (* (aref mat-A 1 0)
		       (aref mat-A 2 1))
		    (* (aref mat-A 1 1)
		       (aref mat-A 2 0)))))
    det))

(defun mat4-transpose (mat-A)
  (let ((ret-mat (make-mat4)))
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (aref ret-mat j i) (aref mat-A i j))))
    ret-mat))

(defun mat4-adj (mat-A)
  (let ((trans-mat (mat4-transpose mat-A))
	(ret-mat (make-mat4)))
    (dotimes (ir 4)
      (dotimes (jr 4)
	(let ((3x3-mat (make-array '(3 3))))
	  (let ((i3 0)
		(j3 0))
	    (dotimes (i 4)
	      (dotimes (j 4)
	      	(if (and (not (eq i ir))
			 (not (eq j jr)))
		    (progn (setf (aref 3x3-mat i3 j3) (aref trans-mat i j))
			   (incf i3)))
		(if (eq i3 3)
		    (setf i3 0
			  j3 (1+ j3)))
		)))
	  (let ((det (mat3-det 3x3-mat)))
	    (if (oddp (+ ir jr))
		(setf det (* det -1)))
	    ;;	    (setf det (* (expt -1 (+ ir jr 2)) det))
	    #|	    (if (and (evenp ir)
	    (oddp jr))
	    (setf det (* det -1)))
	    (if (and (oddp ir)
	    (evenp jr))
	    (setf det (* det -1)))|#
	    (setf (aref ret-mat ir jr) det)))))
    ret-mat))

(defun mat4-det (mat-A)
  (let ((det 0.0)
	(mat-B (make-array '(3 3) :initial-contents (list (list (aref mat-A 1 1) (aref mat-A 1 2) (aref mat-A 1 3))
							  (list (aref mat-A 2 1) (aref mat-A 2 2) (aref mat-A 2 3))
							  (list (aref mat-A 3 1) (aref mat-A 3 2) (aref mat-A 3 3)))))
	(mat-C (make-array '(3 3) :initial-contents (list (list (aref mat-A 0 1) (aref mat-A 0 2) (aref mat-A 0 3))
							  (list (aref mat-A 2 1) (aref mat-A 2 2) (aref mat-A 2 3))
							  (list (aref mat-A 3 1) (aref mat-A 3 2) (aref mat-A 3 3)))))
	(mat-D (make-array '(3 3) :initial-contents (list (list (aref mat-A 0 1) (aref mat-A 0 2) (aref mat-A 0 3))
							  (list (aref mat-A 1 1) (aref mat-A 1 2) (aref mat-A 1 3))
							  (list (aref mat-A 3 1) (aref mat-A 3 2) (aref mat-A 3 3)))))
	(mat-E (make-array '(3 3) :initial-contents (list (list (aref mat-A 0 1) (aref mat-A 0 2) (aref mat-A 0 3))
							  (list (aref mat-A 1 1) (aref mat-A 1 2) (aref mat-A 1 3))
							  (list (aref mat-A 2 1) (aref mat-A 2 2) (aref mat-A 2 3))))))
    (setf det (+ (* (aref mat-A 0 0) (mat3-det mat-B))
		 (* -1 (aref mat-A 1 0) (mat3-det mat-C))
		 (* (aref mat-A 2 0) (mat3-det mat-D))
		 (* -1 (aref mat-A 3 0) (mat3-det mat-E))))
    det))


(defun mat4-inverse (mat-A)
  (let ((ret-mat (mat4-adj mat-A))
	(det (mat4-det mat-A)))
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (aref ret-mat i j) (/ (aref ret-mat i j) det))))
    ret-mat))

#|
;;Code transferred from: https://stackoverflow.com/questions/2624422/efficient-4x4-matrix-inverse-affine-transform
;;                       Answered by "Robin Hilliard" (as in "   var s0 : Number = ...")
(defun mat4-inverse (matA)
(let* ((ret-mat (make-mat4))
(s0 (- (* (aref matA 0 0) (aref matA 1 1)) (* (aref matA 1 0) (aref matA 0 1))))
(s1 (- (* (aref matA 0 0) (aref matA 1 2)) (* (aref matA 1 0) (aref matA 0 2))))
(s2 (- (* (aref matA 0 0) (aref matA 1 3)) (* (aref matA 1 0) (aref matA 0 3))))
(s3 (- (* (aref matA 0 1) (aref matA 1 2)) (* (aref matA 1 1) (aref matA 0 2))))
(s4 (- (* (aref matA 0 1) (aref matA 1 3)) (* (aref matA 1 1) (aref matA 0 3))))
(s5 (- (* (aref matA 0 2) (aref matA 1 3)) (* (aref matA 1 2) (aref matA 0 3))))

(c5 (- (* (aref matA 2 2) (aref matA 3 3)) (* (aref matA 3 2) (aref matA 2 3))))
(c4 (- (* (aref matA 2 1) (aref matA 3 3)) (* (aref matA 3 1) (aref matA 2 3))))
(c3 (- (* (aref matA 2 1) (aref matA 3 2)) (* (aref matA 3 1) (aref matA 2 2))))
(c2 (- (* (aref matA 2 0) (aref matA 3 3)) (* (aref matA 3 0) (aref matA 2 3))))
(c1 (- (* (aref matA 2 0) (aref matA 3 2)) (* (aref matA 3 0) (aref matA 2 2))))
(c0 (- (* (aref matA 2 0) (aref matA 3 1)) (* (aref matA 3 0) (aref matA 2 1))))
(invdet 0))
(setf invdet (/ 1 (- (* s0 c5) (+ (* s1 c4) (+ (* s2 c3) (- (* s3 c2) (+ (* s4 c1) (* s5 c0)))))))

(aref ret-mat 0 0) (* (+ (- (* (aref matA 1 1) c5) (* (aref matA 1 2) c4)) (* (aref matA 1 3) c3)) invdet)
(aref ret-mat 0 1) (* (- (+ (* (aref matA 0 1) c5 -1)  (* (aref matA 0 2) c4)) (* (aref matA 0 3) c3)) invdet)
(aref ret-mat 0 2) (* (+ (- (* (aref matA 3 1) s5) (* (aref matA 3 2) s4)) (* (aref matA 3 3) s3)) invdet)
(aref ret-mat 0 3) (* (- (+ (* (aref matA 2 1) s5 -1) (* (aref matA 2 2) s4)) (* (aref matA 2 3) s3)) invdet)

(aref ret-mat 1 0) (* (- (+ (* (aref matA 1 0) c5 -1) (* (aref matA 1 2) c2)) (* (aref matA 1 3) c1)) invdet)
(aref ret-mat 1 1) (* (+ (- (* (aref matA 0 0) c5 ) (* (aref matA 0 2) c2)) (* (aref matA 0 3) c1)) invdet)
(aref ret-mat 1 2) (* (- (+ (* (aref matA 3 0) s5 -1) (* (aref matA 3 2) s2)) (* (aref matA 3 3) s1)) invdet)
(aref ret-mat 1 3) (* (+ (- (* (aref matA 2 0) s5) (* (aref matA 2 2) s2)) (* (aref matA 2 3) s1)) invdet)

(aref ret-mat 2 0) (* (+ (- (* (aref matA 1 0) c4) (* (aref matA 1 1) c2)) (* (aref matA 1 3) c0)) invdet)
(aref ret-mat 2 1) (* (- (+ (* (aref matA 0 0) c4 -1) (* (aref matA 0 1) c2)) (* (aref matA 0 3) c0)) invdet)
(aref ret-mat 2 2) (* (+ (- (* (aref matA 3 0) s4) (* (aref matA 3 1) s2)) (* (aref matA 3 3) s0)) invdet)
(aref ret-mat 2 3) (* (- (+ (* (aref matA 2 0) s4 -1) (* (aref matA 2 1) s2)) (* (aref matA 2 3) s0)) invdet)

(aref ret-mat 3 0) (* (- (+ (* (aref matA 1 0) c3 -1) (* (aref matA 1 1) c1)) (* (aref matA 1 2) c0)) invdet)
(aref ret-mat 3 1) (* (+ (- (* (aref matA 0 0) c3) (* (aref matA 0 1) c1)) (* (aref matA 0 2) c0)) invdet)
(aref ret-mat 3 2) (* (- (+ (* (aref matA 3 0) s3 -1) (* (aref matA 3 1) s1)) (* (aref matA 3 2) s0)) invdet)
(aref ret-mat 3 3) (* (+ (- (* (aref matA 2 0) s3) (* (aref matA 2 1) s1)) (* (aref matA 2 2) s0)) invdet))
ret-mat))
|#

(defun mat4-scalar (mat-A scalar)
  (let ((ret-mat (make-mat4)))
    (loop for i below 4
	  do (loop for j below 4
		   do (setf (aref ret-mat i j) (* (aref mat-A i j) scalar))))
    ret-mat))

(defun mat4*mat4 (mat-A mat-B)
  (let ((res-mat (make-mat4))
	(res 0))
    (dotimes (i 4)
      (dotimes (j 4)
	(loop for k below 4
	      do (incf res (* (aref mat-A i k)
			      (aref mat-B k j)))
		 ;;	     (format t "MATA: ~D ~D: ~D ~%MATB: ~D ~D ~D~%" i k (aref mat-A i k) k j (aref mat-B k j))
	      )
	(setf (aref res-mat i j) res
	      res 0)))
    res-mat))

(defun mat3*mat3 (mat-A mat-B)
  (let ((res-mat (make-mat3))
	(res 0))
    (dotimes (i 3)
      (dotimes (j 3)
	(loop for k below 3
	      do (incf res (* (aref mat-A i k)
			      (aref mat-B k j)))
		 ;;	     (format t "MATA: ~D ~D: ~D ~%MATB: ~D ~D ~D~%" i k (aref mat-A i k) k j (aref mat-B k j))
	      )
	(setf (aref res-mat i j) res
	      res 0)))
    res-mat))


(defun mat4+mat4 (mat-A mat-B)
  (let ((res-mat (make-mat4)))
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (aref res-mat i j) (+ (aref mat-A i j) (aref mat-B i j)))))
    res-mat))

(defun mat4-mat4 (mat-A mat-B)
  (let ((res-mat (make-mat4)))
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (aref res-mat i j) (- (aref mat-A i j) (aref mat-B i j)))))
    res-mat))

(defun mat4*vec4 (mat-A vecA)
  (let ((vec4? t)
	(res nil)
	(ind-res 0.0))
    (if (eq (car (array-dimensions vecA)) 3)
	(setf res (make-array '(4) :initial-contents (list (aref vecA 0) (aref vecA 1) (aref vecA 2) 1.0))
	      vec4? nil)
	(setf res (make-array '(4) :initial-contents (list (aref vecA 0) (aref vecA 1) (aref vecA 2) (aref vecA 3)))))
    (loop for i below 4
	  do (loop for j below 4
		   do (if (not vec4?)
			  (if (eq j 3)
			      (setf ind-res (+ ind-res (aref mat-A i j)))
			      (setf ind-res (+ ind-res (* (aref mat-A i j) (aref vecA j)))))
			  (setf ind-res (+ ind-res (* (aref mat-A i j) (aref vecA j))))))
	     (setf (aref res i) ind-res
		   ind-res 0.0))
    (if vec4?
	res
	(make-array '(3) :initial-contents (list (aref res 0) (aref res 1) (aref res 2))))))

(defun mat4*vec4-alt (mat-A vecA)
  (let ((vec4? t)
	(res nil)
	(ind-res 0.0))
    (if (eq (car (array-dimensions vecA)) 3)
	(setf res (make-array '(4) :initial-contents (list (aref vecA 0) (aref vecA 1) (aref vecA 2) 1.0))
	      vec4? nil)
	(setf res (make-array '(4) :initial-contents (list (aref vecA 0) (aref vecA 1) (aref vecA 2) (aref vecA 3)))))
    (loop for i below 4
	  do (loop for j below 4
		   do (if (not vec4?)
			  (if (eq j 3)
			      (setf ind-res (+ ind-res (aref mat-A j i)))
			      (setf ind-res (+ ind-res (* (aref mat-A j i) (aref vecA j)))))
			  (setf ind-res (+ ind-res (* (aref mat-A j i) (aref vecA j))))))
	     (setf (aref res i) ind-res
		   ind-res 0.0))
    (if vec4?
	res
	(make-array '(3) :initial-contents (list (aref res 0) (aref res 1) (aref res 2))))))

#|
(defun vec4-scalar (vecA scalar)
)
|#
(defun vec3-scalar (vecA scalar)
  (let ((res-vec (make-array '(3) :initial-contents (list (aref vecA 0) (aref vecA 1) (aref vecA 2)))))
    (loop for i below 3
	  do (setf (aref res-vec i) (* (aref vecA i) scalar)))
    res-vec))

(defun vec3+vec3 (vecA vecB)
  (let ((ret-vec (make-array '(3))))
    (dotimes (i 3)
      (setf (aref ret-vec i) (+ (aref vecA i) (aref vecB i))))
    ret-vec))
