(in-package #:shmup-test)

;;;; This is the game class, that actually handles resources and major
;;;; state changes withing the game


;; Accessory defuns
(defun burst-fire (x y num-shots aim-direction spread recipient-function)
  "burst-fire will pass the valuex x, y, direction, speed into 
recipient-function,"
  (if (< num-shots 1) (return-from burst-fire))
  (when (= num-shots 1) ; 1 or less shots
    (funcall recipient-function x y aim-direction)
    (return-from burst-fire))
  (let ((start-angle (- aim-direction (* spread .5)))
	(step-angle (/ spread (1- (* 1.0 num-shots)))))
	(dotimes (i num-shots)
	  (funcall recipient-function x y (+ start-angle (* step-angle i))))))

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
	      :documentation "Hitbox for defining play area")
   (player-image :accessor game-player-image)
   (bullet-image :accessor game-bullet-image)))

(defmethod initialize-instance :after ((game game) 
				       &key 
					 (width 600)
					 (height 800))
  (setf (game-play-area game)
	(make-instance 'hitbox 
		       :width width 
		       :height height
		       :center-x (/ width 2)
		       :center-y (/ height 2)))
  (dotimes (i 4)
    (add-playerf game
		 (make-instance 'player 
				:x (random (width game))
				:y (random (height game)))))

  ;; Should always be the last function executed in init as it starts SDL
  (sdl:load-library) ;; DO NOT APPEND BELOW THIS LINE
  (sdl:with-init () (sdl:window 
		     (hitbox-width (game-play-area game))
		     (hitbox-height (game-play-area game ))
		     :title-caption "Shmup Test"
		     :DOUBLE-BUFFER T)
		 (setf (sdl:frame-rate) 60)
		 (setf (game-bullet-image game)
		       (sdl-image:load-image 
			"Resources/Sprites/test_shot.png" 
			:image-type :PNG :force t :color-key-at #(0 0)))
		 
		 (setf (game-player-image game)
		       (sdl-image:load-image 
			"Resources/Sprites/ships.gif" 
			:image-type :GIF :force t :color-key-at #(0 0)))
		 (sdl:with-events ()
		   (:quit-event () t)
		   (:idle ()
			  (sdl:clear-display sdl:*black*)
			  (stepf game)
			  (sdl:update-display)))))

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

(defmethod burst-newf ((game game) x y direction)
  (add-enemy-shotf game
		   (make-game-object (floor (/ (width game) 2))
				     (floor (* (height game) .33))
				     :speed 5.0
				     :direction direction
				     :width 16
				     :height 16)))

(defmethod player-shootf ((game game) x y direction)
  (add-player-shotf game
		    (make-game-object x
				      y
				      :speed 10.0
				      :direction direction
				      :width 16
				      :height 16)))


(defmethod draw-gamef ((game game))
  "Main method for drawing game"
  (let ((pt (sdl:point)))
    (loop for i across (game-enemy-shots game) 
       do
	 (setf (aref pt 0) (floor (mover-x i)))
	 (setf (aref pt 1) (floor (mover-y i)))
	 (sdl:draw-surface-at (game-bullet-image game) pt))
    (loop for i across (game-player-shots game) 
       do
	 (setf (aref pt 0) (floor (mover-x i)))
	 (setf (aref pt 1) (floor (mover-y i)))
	 (sdl:draw-surface-at (game-bullet-image game) pt))
    (loop for p across (game-players game)
	 do
	 (sdl:draw-surface-at-* (game-player-image game)
				(floor (mover-x p))
				(floor (mover-y p))))))

(defmethod print-debug ((game game))
  "Debug method called during keypress"
  (when (sdl:get-key-state :sdl-key-f1)
    (format t "player shots: ~a~%" (length (game-player-shots game)))
    (format t "enemy shots: ~a~%" (length (game-enemy-shots game)))
    (format t "enemies: ~a~%" (length (game-enemies game)))
    (format t "players: ~a~%" (length (game-players game)))))
    
(defmethod reset-gamef ((game game))
  "Reset the game to default state"
  (setf (game-state game) 'title)
  (reset-fill-vectorf (game-player-shots game))
  (reset-fill-vectorf (game-enemy-shots game))
  (reset-fill-vectorf (game-enemies game)))

(defmethod reset-fill-vectorf ((vec vector))
  "Set the fill pointer to 0, prepare to stomp over old values"
  (setf (fill-pointer vec) 0))

(defmethod stepf ((game game))
  "Main step function for the game"
  (print-debug game)
  (with-accessors ((players game-players)
		   (player-shots game-player-shots)
		   (enemies game-enemies)
		   (enemy-shots game-enemy-shots)
		   (play-area game-play-area))
      game
    (burst-fire (* (width game) .5)
		(* (height game) .5)
		1
		(random (* 2 PI))
		1
		(lambda (x y direction) 
		  (burst-newf game x y direction)))

    (loop for i across players 
       do (unless (game-object-dead i)
	    (stepf i)))
    (loop for i across player-shots 
       do
	 (when (not (collidep play-area (get-hitbox i)))
	   (setf (game-object-dead i) t))
	 (unless (game-object-dead i)
	    (stepf i)))
    (loop for i across enemies 
       do
	 (when (not(collidep play-area (get-hitbox i)))
	   (setf (game-object-dead i) t))
	 (unless (game-object-dead i)
	    (stepf i)))
    (loop for i across enemy-shots 
       do
	 (when (not (collidep play-area (get-hitbox i)))
	   (setf (game-object-dead i) t))
	 (unless (game-object-dead i)
	    (stepf i)))
  
    ;; game objection interactions here


    ;; cull dead shots and enemies
    (setf enemies (delete-if #'game-object-dead enemies))
    (setf enemy-shots (delete-if #'game-object-dead enemy-shots))
    (setf player-shots (delete-if #'game-object-dead player-shots)))
  (draw-gamef game))
    
