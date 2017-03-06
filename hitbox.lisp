(in-package #:shmup-test)

;;TODO MAKE MAKE-COPY METHOD

(defclass hitbox ()
  ((center-x :accessor hitbox-center-x :initarg :center-x)
   (center-y :accessor hitbox-center-y :initarg :center-y)
   (width    :accessor hitbox-width    :initarg :width)
   (height   :accessor hitbox-height   :initarg :height))
  (:documentation "A hitbox class for collision checking"))

;;; Getters
(defmethod half-width ((object hitbox))  
  (* (hitbox-width object) .5))

(defmethod half-height ((object hitbox)) 
  (* (hitbox-height object) .5))

(defmethod top ((object hitbox)) 
  (- (hitbox-center-y object) (half-height object)))

(defmethod right ((object hitbox)) 
  (+ (hitbox-center-x object) (half-width object)))

(defmethod bottom ((object hitbox)) 
  (+ (hitbox-center-y object) (half-height object)))

(defmethod left ((object hitbox)) 
  (- (hitbox-center-x object) (half-width object)))

(defmethod location ((object hitbox))
  (values (hitbox-center-x object) (hitbox-center-y object)))

(defmethod size ((object hitbox))
  (values (hitbox-center-width object) (hitbox-center-height object)))

;;; Setters
(defmethod set-sizef ((object hitbox) (width number) (height number))
  (setf (hitbox-width object) width)
  (setf (hitbox-height object) height))

(defmethod set-xf ((entity hitbox) (value number))
  (setf (hitbox-center-x entity) value))

(defmethod set-yf ((entity hitbox) (value number))
  (setf (hitbox-center-y entity) value))

(defmethod set-positionf ((object hitbox) (x number)  (y number))
  (setf (hitbox-center-x object) x)
  (setf (hitbox-center-y object) y))


;;; Predicates
(defmethod collidep ((object hitbox) (object2 hitbox))
  (not (or 
	(< (bottom object2) (top object))    ;Above, no collide
	(> (top object2) (bottom object))    ;Below, no collide
	(< (right object2) (left object))    ;Left of, no collide
	(> (left object2) (right object))))) ;Right of, no collide
      





