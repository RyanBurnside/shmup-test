
(in-package #:shmup-test)


;;; Accessors
(defgeneric location (entity)
  (:documentation "Returns the x and y position of a given object as values"))

;;; Mutators
(defgeneric set-directionf (entity value)
  (:documentation "Sets the direction of a given object"))

(defgeneric set-positionf (entity value value2)
  (:documentation "Sets the x and y position of a given object"))

(defgeneric set-speedf (entity value)
  (:documentation "Sets the speed of a given object"))

(defgeneric set-xf (entity value)
  (:documentation "Sets the x of a given object"))

(defgeneric set-yf (entity value)
  (:documentation "Sets the x of a given object"))

(defgeneric stepf (entity)
  (:documentation "Take a step of movement according to internal state"))


;;; Predicates
(defgeneric collidep (entity entity2)
  (:documentation "Returns true if two entities collide else return nil"))

;;; Other

(defgeneric make-copy (entity)
  (:documentation "Returns a freshly created copy of an instance. Good for cloning from a 'gallery' of predesigned instances."

