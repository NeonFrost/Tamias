(defstruct bounding-box
  (x 0)
  x-equation 
  (y 0)
  y-equation
  (width 16)
  width-equation
  (height 16)
  height-equation)

(defstruct t-object
  (x 0)
  (y 0)
  (width 16)
  (height 16)
  (current-cell 0)
  sprite-sheet
  texture
  (vector (make-vector-3d))
  (acceleration (make-vector-3d :z 0))
  (friction 1)
  (mass 1)
  (bounding-box (make-bounding-box)))

(defmacro acceleration-x (object)
  `(vector-3d-x (object-acceleration ,object)))
(defmacro acceleration-y (object)
  `(vector-3d-y (object-acceleration ,object)))
(defmacro acceleration-z (object)
  `(vector-3d-z (object-acceleration ,object)))

(defstruct (entity (:include t-object))
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
  weapon
  armor
  (bg-color +black+)
  (symbol-color +white+)
  (score 0))

(defun test-point-collision (a b c)
  (if (and (>= a b)
	   (<= a c))
      t))

(defun test-bb-collision (entity-a entity-b)
  ;;BB = Bounding Box, bba = Object-a's bounding box, bbb = Object-b's bounding box
  (let* ((bba (t-object-bounding-box entity-a))
	 (bba-x (eval (bounding-box-x bba)))
	 (bba-y (eval (bounding-box-y bba)))
	 (bba-width (eval (bounding-box-width bba)))
	 (bba-height (eval (bounding-box-height bba)))
	 (bbb (t-object-bounding-box entity-b))
	 (bbb-x (eval (bounding-box-x bbb)))
	 (bbb-y (eval (bounding-box-y bbb)))
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

(defun point-within-bounding-box (point bounding-box) ;;point = (x y), bounding-box = (object-bounding-box object)
  (let ((x (car point))
	(y (cadr point))
	(bbx (bounding-box-x bounding-box))
	(bby (bounding-box-y bounding-box))
	(bbw (bounding-box-width bounding-box))
	(bbh (bounding-box-height bounding-box)))
    (if (and (>= x bbx)
	     (<= x bbw)
	     (>= y bby)
	     (<= y bbh))
	t)))

(defun within-bounding-box (x y bounding-box) ;;bounding-box = (object-bounding-box object)
  (let ((bbx (bounding-box-x bounding-box))
	(bby (bounding-box-y bounding-box))
	(bbw (bounding-box-width bounding-box))
	(bbh (bounding-box-height bounding-box)))
    (if (and (>= x bbx)
	     (<= x bbw)
	     (>= y bby)
	     (<= y bbh))
	t)))
