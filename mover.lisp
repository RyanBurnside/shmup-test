;;;; shmup-test.lisp

;;TODO MAKE MAKE-COPY METHOD

(in-package #:shmup-test)
;; Movers only should expose slots x and y directly
;; The rest cause a recalculation of step-x,y

(defclass mover ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (step-x :initform 0 :reader step-x) ;hidden
   (step-y :initform 0 :reader step-y) ;hidden
   (direction :accessor direction :initarg :direction :initform 0)
   (speed :accessor speed :initarg :speed :initform 0))
  (:documentation "Simple movement component, update with mover stepf"))

;;; Methods for mover
(defmethod initialize-instance :after ((object mover) &key)
  (%mover-recalculate object))

;; The function below triggers recalculation of the step-x/y
;; It is often called when dependant slots are mutated
(defmethod %mover-recalculate ((object mover))
  (setf (slot-value object 'step-x)
	(* (cos (direction object)) 
	   (speed object))) ;step-x

  (setf (slot-value object 'step-y) 
	(* (sin (direction object))
	   (speed object)))) ;step-y

(defmethod location ((object mover)) ;Note, "position" is a resrved word
  (values (x object) (y object)))

 (defmethod (setf direction) (val (object mover)) 
   (with-slots (direction) object 
     (setf direction val) 
     (%mover-recalculate object)))

 (defmethod (setf speed) (val (object mover)) 
   (with-slots (speed) object 
     (setf speed val) 
     (%mover-recalculate object)))

(defmethod set-positionf ((object mover) (x number) (y number))
  (setf (x object) x)
  (setf (y object) y)
  (location object))

(defmethod stepf ((object mover))
  (incf (x object) (step-x object))
  (incf (y object) (step-y object)))


;; TODO additional types of mover aka spline also waypoint
