;;;; shmup-test.lisp

;;TODO MAKE MAKE-COPY METHOD

(in-package #:shmup-test)

(defclass game-object (mover)
  ((HP     :reader   game-object-HP 
	   :initarg :HP          
	   :initform 1
	   :documentation "Health points, dead if <= 0")
   (hitbox :accessor game-object-hitbox
	   :initarg :hitbox 
	   :initform nil
	   :documentation "may be nil or a hitbox instance")
   (dead   :accessor game-object-dead 
	   :initform nil
	   :documentation "A simple flag for dead object in game")))

(defmethod %hitbox-recenter ((object game-object))
  ;; Hooks hitbox if not nil and repositions it to center
  (when (typep (game-object-hitbox object) 'hitbox)
    (set-positionf (game-object-hitbox object)
		   (mover-x object) 
		   (mover-y object))))


(defmethod initialize-instance :after ((object game-object) &key)
  (%hitbox-recenter object))


(defmethod %determine-health-dead ((object game-object))
  ;; Sets the dead flag if HP is <= 0
  (when (<= (game-object-HP object) 0.0)
    (setf (game-object-dead object) t)))


;;; Setters
(defmethod set-HPf ((object game-object) (value number))
  ;; If passed a value <= 0 the dead flag is set to true
  (setf (slot-value object 'HP) value)
  (%determine-health-dead object))

(defmethod set-incf-HPf ((object game-object) (value number))
  ;; incf for HP adjustments with dead flag hook
  (incf (slot-value object 'HP) value)
  (%determine-health-dead object))

(defmethod stepf :after ((object game-object))
  ;; Reposition the hitbox
  (%hitbox-recenter object))


;;; Predicates
(defmethod collidep ((object game-object) (object2 game-object))
  (if (and (typep (game-object-hitbox object) 'hitbox) 
	   (typep (game-object-hitbox object2) 'hitbox))

      ;; Both valid hitboxes, return the test results
      (collidep (game-object-hitbox object) (game-object-hitbox object2))

      ;; One or more is Nil, test returns NIL
      nil))


