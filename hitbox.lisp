(in-package #:shmup-test)

;;TODO MAKE MAKE-COPY METHOD

(defclass hitbox ()
  ((center-x :accessor center-x :initarg :center-x)
   (center-y :accessor center-y :initarg :center-y)
   (width    :accessor width    :initarg :width)
   (height   :accessor height   :initarg :height))
  (:documentation "A hitbox class for collision checking"))

;;; Getters
(defmethod half-width ((object hitbox))  
  (* (width object) .5))

(defmethod half-height ((object hitbox)) 
  (* (height object) .5))

(defmethod top ((object hitbox)) 
  (- (center-y object) (half-height object)))

(defmethod right ((object hitbox)) 
  (+ (center-x object) (half-width object)))

(defmethod bottom ((object hitbox)) 
  (+ (center-y object) (half-height object)))

(defmethod left ((object hitbox)) 
  (- (center-x object) (half-width object)))

(defmethod location ((object hitbox))
  (values (center-x object) (center-y object)))

(defmethod size ((object hitbox))
  (values (width object) (height object)))

;;; Setters
(defmethod set-sizef ((object hitbox) (width number) (height number))
  (setf (width object) width)
  (setf (height object) height))

(defmethod set-positionf ((object hitbox) (x number)  (y number))
  (setf (center-x object) x)
  (setf (center-y object) y))


;;; Predicates
(defmethod collidep ((object hitbox) (object2 hitbox))
  (not (or 
	(< (bottom object2) (top object))    ;Above, no collide
	(> (top object2) (bottom object))    ;Below, no collide
	(< (right object2) (left object))    ;Left of, no collide
	(> (left object2) (right object))))) ;Right of, no collide
      





