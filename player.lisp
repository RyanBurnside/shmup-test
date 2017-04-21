(in-package #:shmup-test)

(defparameter *starting-lives* 3)

(defclass player (game-object)
  ((lives         :accessor player-lives :initarg :lives :initform *starting-lives*)
   (score         :accessor player-score :initform 0)
   (shot-timer    :accessor player-shot-timer :initform (make-instance 'ticker :ready-at 3))
   (shot-function :accessor player-shot-function :initarg :shot-function :initform (lambda ())
		  :documentation "Action to take upon shot-timer being ready to fire")
   (move-speed :accessor player-move-speed :initform 3)
   ;; The following should be bound to functions returning true or false
   (leftp  :accessor player-left-p  :initform (lambda() nil))
   (rightp :accessor player-right-p :initform (lambda() nil))
   (upp    :accessor player-up-p    :initform (lambda() nil))
   (downp  :accessor player-down-p  :initform (lambda() nil))
   (firep  :accessor player-fire-p  :initform (lambda() nil))
   (bombp  :accessor player-bomb-p  :initform (lambda() nil))))



;; Mutators

(defmethod updatef ((player player))
  ;; Update inputs
  (update-controllsf player)
  ;; Move
  (stepf player)
  ;; Reset speed to 0.0
  (set-speedf player 0.0)
  ;; Handle shooting timer
  (when (and (readyp (player-shot-timer player)) 
	     (funcall (player-fire-p player)))
    (funcall (player-shot-function player)))

  (if (readyp (player-shot-timer player))
      (resetf (player-shot-timer player))
      (tickf (player-shot-timer player))))

(defmethod update-controllsf ((player player))
  (let* ((l (if (funcall (player-left-p player)) 1 0))
	 (r (if (funcall (player-right-p player)) 1 0))
	 (u (if (funcall (player-up-p player)) 1 0))
	 (d (if (funcall (player-down-p player)) 1 0))
	 (x-speed (- r l))
	 (y-speed (- d u))
	 (direction (atan y-speed x-speed)))
    
    (set-directionf player (atan y-speed x-speed))
    
    ;; Set the speed to travelling speed if any direction pressed
    (if (> (+ l r u d) 0.0)
	(set-speedf player (player-move-speed player))
	(set-speedf player 0.0))))

(defmethod resetf ((player player))
  (resetf (player-shot-timer player))
  (set-speedf player 0.0)
  (setf (player-lives player) *starting-lives*)
  (setf (player-score player) 0))
