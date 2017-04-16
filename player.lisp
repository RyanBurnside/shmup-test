(in-package #:shmup-test)

(defparameter *starting-lives* 3)


(defclass player (game-object)
  ((lives      :accessor player-lives :initarg :lives :initform *starting-lives*)
   (score      :accessor player-score :initarg :lives :initform 0)
   (shot-timer :accessor player-shot-timer :initform (make-instance 'ticker))
   (shot-function :accessor player-shot-function :initarg :shot-function :initform (lambda ())
		  :documentation "Action to take upon shot-timer being ready to fire")))

(defmethod resetf ((player player))
  (setf (player-lives player) *starting-lives*)
  (setf (player-score player) 0))
