(in-package :shmup-test)

;;TODO MAKE MAKE-COPY METHOD

(defclass ticker ()
  ((value :accessor ticker-value :initform 0)
   (ready-at :accessor ticker-ready-at :initform 0 :initarg :ready-at))
  (:documentation "A class that ticks from 0 to ready-at then stops"))

(defmethod ready ((ticker ticker))
  (= (ticker-value ticker)
     (ticker-ready-at ticker)))

(defmethod tickf ((ticker ticker))
  (unless (ready ticker)
    (incf (ticker-value ticker))))

(defmethod resetf ((ticker ticker))
  (setf (ticker-value ticker) 0))

(defclass ticker-action (ticker)
  ((on-ready :accessor ticker-on-ready  :initform (lambda()())
	     :initarg :on-ready :documentation "Action done on trigger"))
   (:documentation "Calls action on-ready then resets timer to 0"))

(defmethod tickf :after ((ticker-action ticker-action))
  (when (ready ticker-action)
    (funcall (ticker-on-ready ticker-action))
    (resetf ticker-action)))


