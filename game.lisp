(in-package #:shmup-test)

;;;; This is the game class, that actually handles resources and major
;;;; state changes withing the game


(defclass game ()
  ((state        :accessor game-state :initform 'title)

   (players :accessor game-players
	    :initform (make-array  1 :fill-pointer 0 :adjustable t)
	    :documentation "A vector allowed to grow as needed")

   (player-shots :accessor game-player-shots
		 :initform (make-array  1 :fill-pointer 0 :adjustable t)
		 :documentation "A vector allowed to grow as needed")

   (enemies  :accessor game-enemies
		   :initform  (make-array 1 :fill-pointer 0 :adjustable t)
		   :documentation "A vector allowed to grow as needed")

   (enemy-shots  :accessor game-enemy-shots 
		 :initform  (make-array 1 :fill-pointer 0 :adjustable t)
		 :documentation "A vector allowed to grow as needed")
   (play-area :accessor game-play-area :initform (make-instance 'hitbox 
								:center-x 240
								:center-y 320
								:width 480
								:height 640)
	      :documentation "Hitbox for defining play area")))

(defmethod initialize-instance :after ((game game) 
				       &key 
					 (width 600)
					 (height 800))
  (setf (game-play-area game)
	(make-instance 'hitbox 
		       :width width 
		       :height height
		       :center-x (/ width 2)
		       :center-y (/ height 2))))

;;; getters
(defmethod width ((game game))
  (hitbox-width (game-play-area game)))

(defmethod height ((game game))
  (hitbox-height (game-play-area game)))


;;; Mutators
(defmethod add-playerf ((game game) (player player))
  (vector-push-extend player (game-players game)))

(defmethod add-enemyf ((game game) (enemy enemy))
  (vector-push-extend enemy (game-enemies game)))

(defmethod add-enemy-shotf ((game game) (enemy-shot game-object))
  (vector-push-extend enemy-shot (game-enemy-shots game)))

(defmethod add-player-shotf ((game game) (player-shot game-object))
  (vector-push-extend player-shot (game-player-shots game)))


(defmethod reset-gamef ((game game))
  (setf (game-state game) 'title)
  (reset-fill-vectorf (game-player-shots game))
  (reset-fill-vectorf (game-enemy-shots game))
  (reset-fill-vectorf (game-enemies game)))

(defmethod reset-fill-vectorf ((vec vector))
  "Set the fill pointer to 0, prepare to stomp over old values"
  (setf (fill-pointer vec) 0))

(defmethod stepf ((game game))
  ;; move 
  (with-accessors ((players game-players)
		   (player-shots game-player-shots)
		   (enemies game-enemies)
		   (enemy-shots game-enemy-shots))
                  game
    (loop for i across players 
       do (unless (game-object-dead i)
	    (stepf i)))
    (loop for i across player-shots 
       do
	 (when (not (collidep (game-play-area game) (get-hitbox i)))
	   (setf (game-object-dead i) t))
	 (unless (game-object-dead i)
	    (stepf i)))
    (loop for i across enemies 
       do
	 (when (not(collidep (game-play-area game) (get-hitbox i)))
	   (setf (game-object-dead i) t))
	 (unless (game-object-dead i)
	    (stepf i)))
    (loop for i across enemy-shots 
       do
	 (when (not (collidep (game-play-area game) (get-hitbox i)))
	   (setf (game-object-dead i) t))
	 (unless (game-object-dead i)
	    (stepf i)))
  
    ;; game objection interactions here


    ;; cull dead shots and enemies
    (setf enemies (delete-if #'game-object-dead enemies))
    (setf enemy-shots (delete-if #'game-object-dead enemy-shots))
    (setf player-shots (delete-if #'game-object-dead player-shots))))
    
