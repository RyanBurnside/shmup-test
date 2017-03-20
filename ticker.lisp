(in-package :shmup-test)

;;TODO MAKE MAKE-COPY METHOD

(defclass ticker ()
  ((value :accessor ticker-value :initform 0 :initarg :value)
   (paused :accessor ticker-paused :initform nil :initarg :paused))
  (:documentation "A class that ticks with game steps then resets"))

(defmethod pause ((object ticker))
  (setf (paused object) t))

(defmethod resume ((object ticker))
  (setf (paused object) nil))

(defmethod tick-up ((object ticker))
  (unless paused
    (incf (paused object))))

(defmethod tick-down ((object ticker))
  (unless paused
    (decf (paused object))))
