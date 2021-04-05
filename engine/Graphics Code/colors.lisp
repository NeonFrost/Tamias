(defpackage tamias.colors
  (:use :cl)
  (:export +black+
	   +white+
	   +pastel-grey+
	   +dark-pastel-grey+
	   +pastel-dark-grey+

	   +pastel-gray+                   
	   +dark-pastel-gray+                
	   +pastel-dark-gray+                

	   +cream+                   
	   +red+               
	   +dark-red+               
	   +green+               
	   +blue+               
	   +yellow+                   
	   +pastel-red+                   
	   +dark-pastel-red+                 
	   +pastel-green+                   
	   +pastel-blue+                   
	   +pastel-yellow+                   
	   +pastel-pink+                   
	   +dark-pastel-pink+

	   +aqua+                   
	   +purple-jade+                  
	   +cobalt+                 
	   +yellow-zinc+                 
	   +chalk-white+                   
	   +brown+                
	   +natural-green+                  
	   +dark-natural-green+                
	   +navy-blue+                
	   +steel-blue+                  
	   +dark-steel-blue+                
	   +orange+))
(in-package :tamias.colors)

(defmacro def-color (color-name r g b)
  `(defparameter ,color-name (list ,r ,g ,b 255)))
(defmacro def-hex-color (color-name hex-code)
  `(defparameter ,color-name (tamias.string:parse-hex-color ,hex-code :return-type 'list)))
(defparameter +black+ (list 0 0 0 255))
(defparameter +white+ (list 255 255 255 255))
(defparameter +pastel-grey+ (list 207 207 196 255))
(defparameter +dark-pastel-grey+ (list 52 52 40 255))
(defparameter +pastel-dark-grey+ (list 52 52 40 255))

(defparameter +pastel-gray+ (list 207 207 196 255))
(defparameter +dark-pastel-gray+ (list 52 52 40 255))
(defparameter +pastel-dark-gray+ (list 52 52 40 255))

(defparameter +cream+ (list 255 253 208 255))
(defparameter +red+ (list 255 0 0 255))
(defparameter +dark-red+ (list 127 0 0 255))
(defparameter +green+ (list 0 255 0 255))
(defparameter +blue+ (list 0 0 255 255))
(defparameter +yellow+ (list 252 241 125 255))
(defparameter +pastel-red+ (list 253 139 139 255))
(defparameter +dark-pastel-red+ (list 160 69 69 255))
(defparameter +pastel-green+ (list 180 242 156 255))
(defparameter +pastel-blue+ (list 167 236 251 255))
(defparameter +pastel-yellow+ (list 255 255 186 255))
(defparameter +pastel-pink+ (list 255 209 220 255))
(defparameter +dark-pastel-pink+ (tamias.string:parse-hex-color "e5b3b7" :return-type 'list))

(defparameter +aqua+ (list 160 217 208 255))
(defparameter +purple-jade+ (list 225 61 147 255))
(defparameter +cobalt+ (list 5 128 198 255))
(defparameter +yellow-zinc+ (list 227 210 8 255))
(defparameter +chalk-white+ (list 245 245 245 255))
(defparameter +brown+ (list 93 67 45 255))
(defparameter +natural-green+ (list 93 161 113 255))
(defparameter +dark-natural-green+ (list 46 80 56 255))
(defparameter +navy-blue+ (list 45 71 93 255))
(defparameter +steel-blue+ (list 70 130 180 255))
(defparameter +dark-steel-blue+ (list 35 65 90 255))
(defparameter +orange+ (list 255 165 0 255))
