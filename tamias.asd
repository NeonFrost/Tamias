(defsystem :tamias
  :author "Brandon Blundell | Neon Frost"
  :maintainer "Brandon Blundell | Neon Frost"
  :license "MIT"
  :version "0.7"
  :description "A game engine built and designed in Common LISP."
  :build-operation program-op
  :build-pathname "VIDYA"
  :entry-point "cl-user::main"
  :depends-on (:sdl2
	       :sdl2-image
	       :sdl2-mixer
	       :sdl2-ttf)
  :components ((:module "engine"
			:serial t
			:components
			((:module "core"
				  :serial t
				  :components
				  ((:file "tamias" :type "cl")
				   (:file "states" :type "cl")))
			 (:file "macros" :type "cl")
			 (:module "Graphics Code"
				  :serial t
				  :components
				  ((:file "string" :type "cl")
				   (:file "colors")
				   (:file "library")
				   (:file "Menus")))
			 (:module "logic"
				  :serial t
				  :components
				  ((:file "entities" :type "cl")
				   (:file "vectors")
				   (:file "math" :type "cl")
				   (:file "2D-particles")
				   #|(:modlue "Physics"
				   :serial t
				   :components|#))
			 (:module "audio"
				  :serial t
				  :components
				  ((:file "music")
				   (:file "sound")))
			 (:module "input"
				  :serial t
				  :components
				  ((:file "mouse" :type "cl")
				   (:file "keyboard" :type "cl")))
			 (:file "loops" :type "cl")
			 (:file "render-engine" :type "cl")
			 (:file "init-assets" :type "cl")
			 (:file "init-main" :type "cl")))))
