(defsystem :demos
  :author "Brandon Blundell | NeonFrost"
  :maintainer "Brandon Blundell | NeonFrost"
  :license "GPL V3"
  :version "0.4"
  :description "Description."
  :depends-on (:sdl2
	       :sdl2-image
	       :sdl2-mixer)
  :components ((:module "demos"
			:serial t
			:components
			((:file "init" :type "cl")
			 (:file "level" :type "cl")
			 (:file "grapple" :type "cl")
			 (:file "render-title-screen" :type "cl")
			 (:file "input" :type "cl")
			 (:file "player" :type "cl")
			 ))))
