(in-package #:shmup-test)

(defclass player (game-object)
  ((lives :accessor player-lives :initarg :lives :initform 3)
   (score :accessor player-score :initarg :lives :initform 0)))
