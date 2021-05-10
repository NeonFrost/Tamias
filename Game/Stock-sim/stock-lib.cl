(defstruct stock
  name
  ticker
  price
  starting-price
  ending-price
  change
  dividend)

(defvar stocks nil)
(defmacro def-stock (ticker name price &key (dividend 0))
  `(push (make-stock :name ,name :ticker ,ticker :price ,price :dividend ,dividend) stocks))
(def-stock "CLF" "Cliff Mining Corp." 7.00)
(def-stock "CIF" "Mfs income" 2.33 :dividend 10)
(def-stock "ZOM" "Zomedica" .133)
(def-stock "SNSS" "Sunessis Pharmaceuticals" .28)
(def-stock "VEON" "Veon Corp" 2.38 :dividend 5.3)
(def-stock "AMD" "Advanced Micro Devices" 53.00 :dividend 3.2)

(defun update-stocks ()
  (loop for stock in stocks
     do (let ((percentile (random 4))
	      (direction (random 2)))
	  (if (= direction 0)
	      (setf percentile (* -1 percentile)))
	  (incf (stock-price stock) (/ (* (stock-price stock) percentile) 100))
	  (setf (stock-price stock) (* 1.0 (/ (round (* (stock-price stock) 10000)) 10000)))
	  (setf (stock-change stock) percentile)
	  )))
