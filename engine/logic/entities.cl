#|
File for the 'entity' structures
|#
(defstruct game-user
  (name "Nika")
  (level "Forest")
  (room 1)
  (inventory (make-inventory))
  started-quests
  finished-quests
  kills
  items-collected ;; -> '((health-potion 5) (scaler-teeth 10))
  )

(defstruct bounding-box
  (x 0)
  (y 0)
  (width 16)
  (height 16))

(defstruct entity
  name
  (attack 10)
  (attack-mod 0)
  (defense 0)
  (defense-mod 0)
  (range-attack 10)
  (range-attack-mod 0)
  (magic-attack 10)
  (magic-attack-mod 0)
  (magic-defense 10)
  (magic-defense-mod 0)
  (agility 1) ;;;;Higher goes first, lower goes last
  (agility-mod 0)
  (speed 1) ;;;;in Rogue-like engines, this will be how many spaces an entity can move
  (speed-mod 0)
  (dodge 1)
  (dodge-mod 0)
  (hp 40)
  (max-hp 40)
  (mp 40)
  (max-mp 40)
  (xp 0)
  (level 1)
  (elemental 'entity)
  (status 'alive)
  extra-effect ;;;;i.e.
  current-action
  (symbol "E")
  (line-of-sight 10) ;meaning, it can see or ''see'' x amount of tiles away
  (x 0)
  (y 0)
  (width 16)
  (height 16)
#|  (position '((:x 0)
	      (:y 0)))|#
  weapon
  armor
  sprite-sheet
  (bg-color +black+)
  (symbol-color +white+)
  (current-cell 0)
  (bounding-box (make-bounding-box))
  vector
  (score 0)
  )

#|(defmacro entity-x (entity)
  `(cadr (assoc :x (entity-position ,entity))))
(defmacro entity-y (entity)
  `(cadr (assoc :y (entity-position ,entity))))|#

(defun test-collision (entity-a entity-b)
  (let* ((bba (entity-bounding-box entity-a))
	 (bba-x (bounding-box-x bba))
	 (bba-y (bounding-box-y bba))
	 (bba-width (eval (bounding-box-width bba)))
	 (bba-height (eval (bounding-box-height bba)))
	 (bbb (entity-bounding-box entity-b))
	 (bbb-x (bounding-box-x bbb))
	 (bbb-y (bounding-box-y bbb))
	 (bbb-width (eval (bounding-box-width bbb)))
	 (bbb-height (eval (bounding-box-height bbb))))
    (if (or (and (>= bba-x bbb-x)
		 (<= bba-x (+ bbb-x bbb-width)))
	    (and (<= bba-x bbb-x)
		 (>= (+ bba-x bba-width) bbb-x)))
	(if (or (and (>= bba-y bbb-y)
		     (<= bba-y (+ bbb-y bbb-height)))
		(and (<= bba-y bbb-y)
		     (>= (+ bba-y bba-height) bbb-y)))
	    t))))
