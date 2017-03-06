;;;; shmup-test.lisp

;;TODO MAKE MAKE-COPY METHOD

(in-package #:shmup-test)

;; Movers only should expose slots x and y directly
;; The rest cause a recalculation of step-x,y
(defclass mover ()
  ((x :accessor mover-x :initarg :x :initform 0)
   (y :accessor mover-y :initarg :y :initform 0)
   (step-x :initform 0 :reader mover-step-x) ;hidden
   (step-y :initform 0 :reader mover-step-y) ;hidden
   (direction :reader mover-direction :initarg :direction :initform 0)
   (speed :reader mover-speed :initarg :speed :initform 0))
  (:documentation "Simple movement component, update with mover.stepf"))

;;; Methods for mover
(defmethod initialize-instance :after ((object mover) &key)
  (%mover-recalculate object))

;; The function below triggers recalculation of the step-x/y
;; It is often called when dependant slots are mutated
(defmethod %mover-recalculate ((object mover))
  (setf (slot-value object 'step-x)
	(* (cos (mover-direction object)) 
	   (mover-speed object))) ;step-x

  (setf (slot-value object 'step-y) 
	(* (sin (mover-direction object))
	   (mover-speed object)))) ;step-y

(defmethod location ((object mover)) ;Note, "position" is a register word
  (values (mover-x object) (mover-y object)))

(defmethod set-directionf ((object mover) (val number))
  (setf (slot-value object 'direction) val)
  (%mover-recalculate object)
  (mover-direction object))

(defmethod set-speedf ((object mover) (val number))
  (setf (slot-value object 'speed) val)
  (%mover-recalculate object)
  (mover-speed object))

(defmethod set-xf ((object mover) (val number))
  (setf (mover-x object) val)
  (mover-x object))

(defmethod set-yf ((object mover) (val number))
  (setf (mover-y object) val)
  (mover-y object))

(defmethod set-positionf ((object mover) (x number) (y number))
  (mover-set-xf object x)
  (mover-set-yf object y)
  (location object))

(defmethod stepf ((object mover))
  (incf (slot-value object 'x) (mover-step-x object))
  (incf (slot-value object 'y) (mover-step-y object)))


;; TODO additional types of mover aka spline also waypoint
