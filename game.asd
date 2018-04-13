(defsystem :game
  :author "Brandon Blundell"
  :maintainer "Brandon Blundell"
  :license "GPL v3"
  :version "0.9"
  :description "Description."
  :depends-on (:sdl2
	       :sdl2-image
	       :sdl2-mixer)
  :components ((:file "values" :type "cl")
	       (:file "states" :type "cl")
	       (:module "engine"
			:serial t
			:components
			((:module "Graphics Code"
				  :serial t
				  :components
				  ((:file "colors")
				   (:file "string" :type "cl")
				   (:file "library")
				   (:module "Screens"
					    :serial t
					    :components
					    ((:file "Menus")))))
			 (:module "logic"
				  :serial t
				  :components
				  ((:file "entities" :type "cl")
				   (:file "vectors")
				   (:file "math" :type "cl")
				   (:file "lib" :type "cl")
				   (:file "move-platformer" :type "cl")))
			 (:module "audio"
				  :serial t
				  :components
				  ((:file "music")))
			 (:module "input"
				  :serial t
				  :components
				  ((:file "mouse" :type "cl")
				   (:file "keyboard" :type "cl")))
			 (:file "loops" :type "cl")
			 (:file "render-engine" :type "cl")))
	       (:module "Game"
			:serial t
			:components
			((:file "init" :type "cl")
			 (:file "level" :type "cl")
			 (:file "grapple" :type "cl")
			 (:file "render-title-screen" :type "cl")
			 (:file "input" :type "cl")
			 (:file "player" :type "cl")
			 ))
	       (:file "init-assets" :type "cl")
	       (:file "Main")))
