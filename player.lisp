(in-package #:shmup-test)

(defparameter *starting-lives* 3)

(defclass player (game-object)
  ((lives         :accessor lives 
		  :initarg :lives 
		  :initform *starting-lives*)
   (score         :accessor score :initform 0)
   (shot-timer    :accessor shot-timer 
		  :initform (make-instance 'ticker :ready-at 3))
   (shot-function :accessor shot-function 
		  :initarg :shot-function 
		  :initform (lambda ())
		  :documentation "Action to take upon shot-timer being ready to fire")
   (move-speed :accessor move-speed :initform 3)
   ;; The following should be bound to functions returning true or false
   (leftp  :accessor left-p  :initform (lambda() nil))
   (rightp :accessor right-p :initform (lambda() nil))
   (upp    :accessor up-p    :initform (lambda() nil))
   (downp  :accessor down-p  :initform (lambda() nil))
   (firep  :accessor fire-p  :initform (lambda() nil))
   (bombp  :accessor bomb-p  :initform (lambda() nil))))


(defmethod modify-entity ((entity player))
;; set the player's rate of fire to 5. we can change this while the game is running, and then hit C-c C-c to see the
;; change happen immediately in game.
(setf (move-speed entity) 15.0)
;; you probably want to make an entity class and make the player have this as a parent. then this method would look
;; like (entity entity) to change something common to all entities.
)

;; Mutators
(defmethod updatef ((player player))
  ;; Update inputs
  (update-controllsf player)
  ;; Move
  (stepf player)
  ;; Reset speed to 0.0
  (setf (speed player) 0.0)
  ;; Handle shooting timer
  (when (and (readyp (shot-timer player)) 
	     (funcall (fire-p player)))
    (funcall (shot-function player)))

  (if (readyp (shot-timer player))
      (resetf (shot-timer player))
      (tickf (shot-timer player))))

(defmethod update-controllsf ((player player))
  (let* ((l (if (funcall (left-p player)) 1 0))
	 (r (if (funcall (right-p player)) 1 0))
	 (u (if (funcall (up-p player)) 1 0))
	 (d (if (funcall (down-p player)) 1 0))
	 (x-speed (- r l))
	 (y-speed (- d u))
	 (direction (if (not (and (= x-speed 0) (= y-speed 0)))
			(atan y-speed x-speed)
			0.0)))
    
    (setf (direction player) (atan y-speed x-speed))
    
    ;; Set the speed to travelling speed if any direction pressed
    (if (> (+ l r u d) 0.0)
	(setf (speed player) (move-speed player))
	(setf (speed player) 0.0))))


(defmethod resetf ((player player))
  (resetf (shot-timer player))
  (setf (speed player) 0.0)
  (setf (lives player) *starting-lives*)
  (setf (score player) 0))
