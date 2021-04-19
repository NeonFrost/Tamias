(defvar world-matrix (make-mat4))
;;(setf world-matrix (make-mat4))
;;(setf (aref world-matrix 2 3) 0.0)

(defun get-offset-matrix (bones parent-index)
  (let ((ret-mat (tja-bone-bind-matrix (aref bones parent-index)))
	(inv-bind-mat (mat4-inverse (tja-bone-bind-matrix (aref bones parent-index))))
	(cur-bone (aref bones parent-index)))
;;    (setf ret-mat (mat4*mat4 ret-mat inv-bind-mat))
    (if (tja-bone-parent cur-bone)
	(setf ret-mat (mat4*mat4 ret-mat (get-offset-matrix bones (tja-bone-parent cur-bone)))))
    ret-mat))

(defun get-trans-matrix (bones frames parent-index keyframe)
  (let ((ret-mat (make-mat4))
	(frame-mat (aref frames keyframe parent-index))
	(inv-bind-mat (mat4-inverse (tja-bone-bind-matrix (aref bones parent-index))))
	(cur-bone (aref bones parent-index)))
    (setf ret-mat (mat4*mat4 inv-bind-mat frame-mat))
    (if (tja-bone-parent cur-bone)
	(setf ret-mat (mat4*mat4 ret-mat (get-trans-matrix bones frames (tja-bone-parent cur-bone) keyframe))))
    ret-mat))


(defun set-child-transform (model trans-mat child-index trans-bones)
  (let ((child-mat (aref trans-bones child-index)))
    (setf child-mat (mat4*mat4 child-mat trans-mat))
    (setf (aref trans-bones child-index) child-mat)
    (if (tja-bone-children (aref (tja-data-bones (model-animation model)) child-index))
	(loop for child in (tja-bone-children (aref (tja-data-bones (model-animation model)) child-index))
	   do (set-child-transform model child-mat child trans-bones)))))


#|


SOME WUICL: NOTES !@!!!!!

Given a transformation matrix, it is (mat4*mat4 original-mat trans-mat) I think


Ok, so, I thought I could be lazy in doing all this stuff
No. I can't. 
From Blender, I need: Bones: Head, Tail, rotation, location, scale, and everything else in between

|#


;;So calc-frame-mat here is wrong, afaik
;;I'm going to make an entire file dedicated to bones and a file for bone animation

(defun calc-frame-mat (model bone-ind trans-bones)
  (let ((inv-bind nil)
	(frame-mat nil)
	(bind-mat nil)
;;	(ident-mat (make-mat4))
;;	(parent-bind-mat (make-mat4))
;;	(parent-pose-mat (make-mat4))
;;	(offset-matrix (make-mat4));; (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) bone-ind)))
	(trans-mat (make-mat4)))
    (setf inv-bind (mat4-inverse (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) bone-ind))))
    (setf bind-mat (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) bone-ind)))
    (setf frame-mat (aref (tja-data-frames (model-animation model)) (model-key model) bone-ind))
    (if (< (model-key model) (1- (car (array-dimensions (tja-data-frames (model-animation model))))))
	(progn (setf frame-mat (mat4+mat4 frame-mat (mat4-scalar (calculate-frame-transformation frame-mat
												 (aref (tja-data-frames (model-animation model)) (1+ (model-key model)) bone-ind)
												 (aref (tja-data-timing (model-animation model)) (model-key model))
												 (aref (tja-data-timing (model-animation model)) (1+ (model-key model))))
								 (- (model-frame model) (aref (tja-data-timing (model-animation model)) (model-key model))))))
	       (if (> (model-frame model) (aref (tja-data-timing (model-animation model)) (1+ (model-key model))))
		   (incf (model-key model))))
	(if (>= (model-frame model) (aref (tja-data-timing (model-animation model)) (model-key model)))
	    (setf (model-key model) 0
		  (model-frame model) 1.0)))
;;    (if (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind))
;;	(setf parent-bind-mat (mat4-inverse (tja-bone-bind-matrix (aref (tja-data-bones (model-animation model)) (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind)))))
;;	      parent-pose-mat (mat4-inverse (aref (tja-data-frames (model-animation model)) (1+ (model-key model)) (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind))))))
;;    (setf frame-mat (mat4*mat4 frame-mat (aref trans-bones bone-ind)))
#|    (if (tja-bone-parent (aref (tja-data-bones (model-animation model)) bone-ind))
	(progn (setf offset-matrix (get-offset-matrix (tja-data-bones (model-animation model))
						      bone-ind)
		     trans-mat (get-trans-matrix (tja-data-bones (model-animation model))
						 (tja-data-frames (model-animation model))
						 bone-ind
						 (model-key model)))))
    (if (tja-bone-children (aref (tja-data-bones (model-animation model)) bone-ind))
	(loop for child in (tja-bone-children (aref (tja-data-bones (model-animation model)) bone-ind))
	     do (set-child-transform model (mat4*mat4 frame-mat inv-bind) child trans-bones)))
    (setf offset-matrix (mat4-inverse offset-matrix))|#
    #|
    So, what I may have to do is go through the parent of each parent bone, get their transform matrices, and apply their transforms to the current bone
    I'm not 100% sure as of right now, but that's something to look into
|#
;;    (setf trans-mat (mat4*mat4 t-mat frame-mat))
;;    (setf trans-mat (mat4*mat4 parent-mat trans-mat))
;;    (mat4*mat4 t-mat frame-mat)
;;    (mat4*mat4 t-mat (mat4*mat4 inv-bind frame-mat))
    ;;    (mat4*mat4 t-mat trans-mat)
    ;;    (mat4*mat4 offset-matrix (mat4*mat4 frame-mat inv-bind))
    (setf trans-mat frame-mat)
;;    (setf trans-mat (mat4*mat4 (mat4*mat4 frame-mat parent-pose-mat) (mat4-inverse (mat4*mat4 bind-mat parent-bind-mat))))
    (setf trans-mat (mat4*mat4 world-matrix (mat4*mat4 trans-mat inv-bind)))
;;    (mat4*mat4 trans-mat offset-matrix)
;;    offset-matrix
;;    (mat4*mat4 frame-mat inv-bind)
;;    (mat4*mat4 trans-mat offset-matrix)
;;    (mat4*mat4 offset-matrix trans-mat)
    ))

(defvar obj-mat (make-mat4))

(defun calc-frame-vals (model pos-ind vec trans-bones)
  (let ((return-vec (make-array '(3)))
	(tmp-vec (make-array '(3)))
	(trans-mat (make-mat4))
	(normalized-weight 0.0))
    (setf (aref trans-mat 0 0) 0.0
	  (aref trans-mat 1 1) 0.0
	  (aref trans-mat 2 2) 0.0
	  (aref trans-mat 3 3) 0.0)
    (loop for weight in (aref (model-weights model) pos-ind)
       do (incf normalized-weight (cadr weight)))
    (setf normalized-weight (/ 1.0 normalized-weight))
    (if (< normalized-weight 1.0)
	(setf normalized-weight 1.0))
    (loop for weight in (reverse (aref (model-weights model) pos-ind))
       do (let ((weighted-mat (aref trans-bones (car weight))))
;;	    (mat4-scalar (aref trans-bones (car weight)) (* (cadr weight) normalized-weight))))
	    (setf tmp-vec (mat4*vec4 weighted-mat vec))
	    (setf tmp-vec (vec3-scalar tmp-vec (* (cadr weight) normalized-weight)))
	    (setf return-vec (vec3+vec3 return-vec tmp-vec))))
;;	    (setf trans-mat (mat4+mat4 trans-mat (mat4-scalar weighted-mat normalized-weight)))))
;;    (setf return-vec (mat4*vec4 trans-mat vec))
    return-vec
    ))
