(in-package #:shmup-test)


(defclass emitter (ticker)
  ((parent-x :accessor parent-x :initform 0)

   (parent-y :accessor parent-y :initform 0)

   (action-map :accessor action-map
	       :initarg :action-map
	       :initform (make-hash-table :test #'equalp)
	       :documentation "A hash with int keys, burst shot parameters 
mapped to key")

   (shot-push-func :accessor shot-push-func
		   :initarg :shot-push-func
		   :initform (lambda (&key x y aim speed num-shots spread))
		   :allocation :class
		   :documentation "Function to push x y speed direction spread 
into for a new shot to be created")

   (aim-player-func :accessor aim-player-func
		    :initarg :aim-player-func
		    :initform (lambda (x y) (* PI .5))
		    :allocation :class
		    :documentation "Function to pass the emitter's x and y, 
and get the player's direction for aiming")

   (offset-x :accessor offset-x :initarg :offset-x :initform 0
	     :documentation "Mounted position relative to enemy object")

   (offset-y :accessor offset-y :initarg :offset-y :initform 0
	     :documentation "Mounted position relative to enemy object")

   (repeating :accessor repeating :initarg :repeating :initform nil
	      :documentation "Reset timer upon having no actions left to take")))


(defmethod get-largest-key ((emitter emitter))
  "Returns 0 if no key exists else returns largest key"
  (let ((val (loop for key being the hash-keys of (action-map emitter) collect key)))
    (if val
	(apply #'min val)
	0)))

(defmethod push-burst ((emitter emitter) &key (step 0) (relative t) num-shots speed spread (aim (* PI .5)))
  "Inserts a burst parameter list into an emitter, 
aim may be a direction or :player to call the assigned player targeting 
function. Step is the current index which is inserted by adding on to the last 
index if relative is t otherwise it uses the value of step and clobbers any 
previous data held at index step"
  (let* ((largest (get-largest-key emitter))
	 (index (if relative 
		    (+ largest step)
		    step)))
    (setf (gethash index (action-list emitter))
	  (list :num-shots num-shots
		:spread spread
		:speed spead
		:aim aim))))

(defmethod stepf ((emitter emitter))
  (when (gethash (value emitter))
    (let ((h (gethash (value emitter))))
      (funcall (shot-push-func emitter) 
	       :x (+ parent-x offset-x)
	       :y (+ parent-y offset-y)
	       :aim (getf h :aim)
	       :speed (getf h :speed)
	       :spread (getf h :spread)
	       :num-shots (getf h :num-shots))))
  (tickf emitter))
