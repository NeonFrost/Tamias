(defpackage data-base
  (:use :cl)
  (:export data-bases
	   make))
(in-package :data-base)

(defvar data-bases nil)

(defstruct db
  )
  
"Example: Fish database:
Name: Black Bass
Description: The most metal fish of the sea. "
