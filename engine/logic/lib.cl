(defstruct level
  name
  type 
  music
;;;;Not in use with 'non-graphical' rogue-likes  sheet ;A path
  (battle-chance 0)
  creatures
;;;;  world-xy
  sheet-surface
  (rooms (make-hash-table)))

(defstruct area
  name
  array
  texture
  transitions
;;;;  chests
;;;;  shop
  connected-areas
  connected-level 
  spring)

(defmacro define-level (var &key (name "Level") (type 'dungeon) (music "default-song.ogg") (battle-chance 0) (creatures nil))
  (let ((type (write-to-string type)))
    `(defvar ,var (make-level :name ,name :type (intern ,type) :music ,music :battle-chance ,battle-chance :creatures ,creatures))
    ))

(defmacro define-area (hash-symbol level &key (name "Area") (array (make-array '(10 10))) (transitions (list 0 0)) (connected-areas nil) (connected-level nil) (spring (list 5 5)))
  (let ((area (make-area :name name :array array
			 :transitions transitions :spring spring
			 :connected-areas connected-areas
			 :connected-level connected-level)))
    `(setf (gethash ,hash-symbol (level-rooms ,level)) ,area)))

(defstruct quest
  name
  started
  goals
  finished
  type
  reward)
(defstruct goal
  enemy
  to-kill
  killed
  item
  to-acquire
  acquired
  location
  arrived
  person
  talked)
(defmacro define-goal (&key enemy to-kill item to-acquire location person)
  `(make-goal :enemy ,enemy
	      :to-kill (if ,to-kill
			   (+ (cadr (assoc ,enemy (player-kills user))) ,to-kill))
	      :item ,item
	      :to-acquire (if ,to-acquire
			      (+ (cadr (assoc ,item (player-collected-items user))) ,to-acquire))
	      :location ,location
	      :person ,person)
  )
(defmacro define-quest (&key (name "test") goals (type "miscellanious") reward)
  `(push (make-quest :name ,name
		     :goals ,goals
		     :type ,type
		     :reward ,reward) (player-started-quests user)))
(defmacro finish-quest (quest)
  `(setf (quest-finished ,quest) t))

#|(defstruct chest
x
y
cell
contents
opened
)
(defmacro defchest (level area x y cell contents)
  `(if (area-chests (gethash ,area (level-areas ,level)))
       (setf (area-chests (gethash ,area (level-areas ,level)))
	     (append (list (make-chest :x ,x :y ,y :cell ,cell :contents (list ,contents)) (area-chests (gethash ,area (level-areas ,level))))))
       (setf (area-chests (gethash ,area (level-areas ,level)))
	     (list (make-chest :x ,x :y ,y :cell ,cell :contents (list ,contents))))
       ))|#
#|
======================================================================
                            INVENTORY
                              ITEMS
======================================================================
|#

(defstruct inventory
  weapons
  armor
  items
  weight
  max-weight
  max-items
  max-weapons
  max-armor)

(defstruct item
  name
  type 
  (target 'single)
  class
  (can-equip nil)
  (attack 0)
  (defense 0)
  (agility 0)
  (restore 0) 
  (cost 0)
  (amount 0)
  (weight 1)
  (information "")
  (symbol (ascii-to-string 21))
  ;;;;cell
  )

(defmacro defweapon (weapon name class attack defense agility cost weight &key (symbol 24) (information ""))
  `(defstruct (,weapon (:include item (name ,name) (type 'weapon) (class ,class) (attack ,attack)
				 (defense ,defense) (agility ,agility) (weight ,weight)
				 (cost ,cost) (symbol (ascii-to-string ,symbol)) (information ,information)))))
(defmacro defarmor (armor name attack defense agility cost class &key (symbol "[") (information ""))
  `(defstruct (,armor (:include item (name ,name) (type 'armor) (class ,class) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost) (symbol (ascii-to-string ,symbol)) (information ,information)))))

(defmacro defpotion (potion name &key (target 'single) (class 'healing) (type 'potion) (restore 5) (cost 50))
  (let ((type (write-to-string type))
	(class (write-to-string class))
	(target (write-to-string target)))
    `(defstruct (,potion (:include item (name ,name) (target (intern ,target)) (type (intern ,type)) (class (intern ,class)) (restore ,restore) (cost ,cost))))
    ))

(defmacro defsword (sword name attack defense agility cost)
  `(defstruct (,sword (:include item (name ,name) (type 'weapon) (class 'earth) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost)))))
(defmacro defhammer (hammer name attack defense cost)
  `(defstruct (,hammer (:include item (name ,name) (type 'weapon) (class 'metal) (attack ,attack) (defense ,defense) (cost ,cost)))))
(defmacro deffist (fist name attack defense agility cost)
  `(defstruct (,fist (:include item (name ,name) (type 'weapon) (class 'warrior) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost)))))
(defmacro defchain (chain name attack defense agility cost)
  `(defstruct (,chain (:include item (name ,name) (type 'weapon) (class 'ice) (attack ,attack) (defense ,defense) (agility ,agility) (cost ,cost)))))

#|(defmacro defitem-cell (item cell)
`(setf (item-cell ,item) ,cell))|#
#|
======================================================================
                                NPC
                              PLAYER
                             MONSTERS
                              ENTITY
======================================================================
|#
#|
(defun spawn-creature (creature-structure target-point)
  (let ((x (car target-point))
	(y (cadr target-point)))
    (if (and (> y 0)
	     (< y (car (array-dimensions main-map))))
	(if (and (> x 0)
		 (< x (cadr (array-dimensions main-map))))
	    (setf creatures (append creature-structure creatures)
		  (aref enemy-array y x) (last creatures)
		  (cadr (assoc :x (entity-position (last creatures)))) x
		  (cadr (assoc :y (entity-position (last creatures)))) y)
	    (spawn-creature creature-structure (list (1+ (random (- (cadr (array-dimensions main-map)) 2)))
						     y)))
	(spawn-creature creature-structure (list x
						 (1+ (random (- (car (array-dimensions main-map)) 2)))))
	)))
|#
#|
======================================================================
                             SPELLS
======================================================================
|#

(defstruct spell
  (name nil)
  (cost nil)
  (level 1)
  (target 'single)
  (effect 'attack))

(defmacro defspell (var spell-name cost element &key (level-requirement 1) (target 'single) (effect 'attack) (spell-level 1))
  (let ((eff (write-to-string effect))
	(tar (write-to-string target)))
    `(progn (defparameter ,var (make-spell :name ,spell-name :cost ,cost :target (intern ,tar) :effect (intern ,eff) :level ,spell-level))
	    (setf (gethash ,level-requirement (gethash ,element *legal-spells*)) ,var))
    ))


#|
==============================================================================
                                 BATTLE
==============================================================================
|#

(defmacro decrease-hp (target amount)
  `(decf (entity-hp ,target) ,amount))
(defmacro increase-hp (target amount)
  `(incf (entity-hp ,target) ,amount))
(defmacro decrease-mp (target amount)
  `(decf (entity-mp ,target) ,amount))
(defmacro increase-mp (target amount)
  `(incf (entity-mp ,target) ,amount))
(defmacro change-status (target status)
  `(setf (entity-status ,target) ,status))
(defmacro add-xp (entity amount)
  `(incf (entity-xp ,entity) ,amount))

