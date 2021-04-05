(defstruct stock
  name
  Ticker
  price
  starting-price
  ending-price
  change
  dividend)

(defvar stocks nil)
(defmacro def-stock (ticker name price &key dividend)
  `(push (make-stock :name ,name :ticker ,ticker :price ,price :dividend ,dividend) stocks))
(def-stock "CLF" "Cliff Mining Corp." 7.00)
(def-stock "CIF" "Mfs income" 2.33 :dividend 10)
(def-stock "ZOM" "Zomedica" .133)
(def-stock "SNSS" "Sunessis Pharmaceuticals" .28)
(def-stock "VEON" "Veon Corp" 2.38 :dividend 5.3)
(def-stock "AMD" "Advanced Micro Devices" 53.00 :dividend 3.2)
