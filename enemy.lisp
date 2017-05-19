(in-package #:shmup-test)

(defclass enemy (game-object)
  ((ticks :accessor ticks :initform (make-ticker :ready-at -1)
	  :documentation "This counts each game cycle so the enemy can preform 
actions depending on how long it has lived")
   (emitters :accessor emitters :initform '() :initarg :emitters
	     :documentation "A list of emitters to be updated each step")))
		  

(defmethod stepf :after ((enemy enemy))
  ;; Update all emitter positions with new parent x and y
  (loop for i in (emitters enemy) do
       ;; Update emitters' parent x,y
       (setf (parent-x i) (x enemy))
       (setf (parent-y i) (y enemy))
       ;; Update emitters
       (stepf i))
  (tickf (ticks enemy)))
