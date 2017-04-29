(in-package #:shmup-test)
;;;; This is the game class, that actually handles resources and major
;;;; state changes withing the game


;; Accessory defuns
(defun burst-fire (x y num-shots aim-direction spread recipient-function)
  "burst-fire will pass the valuex x, y, direction 
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
  ((state        :accessor state :initform 'title)

   (players :accessor players
	    :initform (make-array  1 :fill-pointer 0 :adjustable t)
	    :documentation "A vector allowed to grow as needed")

   (player-shots :accessor player-shots
		 :initform (make-array  1 :fill-pointer 0 :adjustable t)
		 :documentation "A vector allowed to grow as needed")

   (enemies  :accessor enemies
	     :initform  (make-array 1 :fill-pointer 0 :adjustable t)
	     :documentation "A vector allowed to grow as needed")

   (enemy-shots  :accessor enemy-shots 
		 :initform  (make-array 1 :fill-pointer 0 :adjustable t)
		 :documentation "A vector allowed to grow as needed")

   (play-area :accessor play-area :initform (make-instance 'hitbox 
							   :center-x 120
							   :center-y 160
							   :width 240
							   :height 320)
	      :documentation "Hitbox for defining play area")

   ;; Some sprite related routines
   (screen-buffer :accessor game-screen-buffer
		  :documentation "Screen buffer slot, 240x320"
		  :initform nil)
   (player-image :accessor game-player-image)
   (bullet-images :accessor game-bullet-images)
   (bullet-images-cells :accessor game-bullet-images-cells)))

(defmethod initialize-instance :after ((game game) 
				       &key 
					 (width 600)
					 (height 800))
  (setf (play-area game)
	(make-instance 'hitbox 
		       :width width 
		       :height height
		       :center-x (/ width 2)
		       :center-y (/ height 2)))
  (dotimes (i 2)
    (add-playerf game
		 (make-instance 'player 
				:x (random (width game))
				:y (random (height game)))))

  ;; Bind player 1 actions
  (setf (left-p (aref (players game) 0))
	(lambda () (sdl:get-key-state :sdl-key-left)))
  (setf (right-p (aref (players game) 0))
	(lambda () (sdl:get-key-state :sdl-key-right)))
  (setf (up-p (aref (players game) 0))
	(lambda () (sdl:get-key-state :sdl-key-up)))
  (setf (down-p (aref (players game) 0))
	(lambda () (sdl:get-key-state :sdl-key-down)))
  (setf (fire-p (aref (players game) 0))
	(lambda () (sdl:get-key-state :sdl-key-z)))
  (setf (shot-function (aref (players game) 0))
	(lambda () (burst-fire (x (aref (players game) 0))
			       (y (aref (players game) 0))
			       4
			       (* pi 1.5)
			       (* pi .1)
			       (lambda (x y direction)
				 (player-shootf game x y direction)))))

  ;; Bind player 2 actions
  (setf (left-p (aref (players game) 1))
	(lambda () (sdl:get-key-state :sdl-key-a)))
  (setf (right-p (aref (players game) 1))
	(lambda () (sdl:get-key-state :sdl-key-d)))
  (setf (up-p (aref (players game) 1))
	(lambda () (sdl:get-key-state :sdl-key-w)))
  (setf (down-p (aref (players game) 1))
	(lambda () (sdl:get-key-state :sdl-key-s)))
  (setf (fire-p (aref (players game) 1))
	(lambda () (sdl:get-key-state :sdl-key-lctrl)))
  (setf (shot-function (aref (players game) 1))
	(lambda () (burst-fire (x (aref (players game) 1))
			       (y (aref (players game) 1))
			       4
			       (* pi 1.5)
			       (* pi .1)
			       (lambda (x y direction)
				 (shootf game x y direction)))))


  ;; Should always be the last function executed in init as it starts SDL
  (sdl:load-library) ;; DO NOT APPEND BELOW THIS LINE
  (sdl:with-init () (sdl:window 
		     240
		     320
		     :title-caption "Shmup Test"
		     :DOUBLE-BUFFER T)
		 (setf (sdl:frame-rate) 60)
		 ;; Create screen buffer 240x320
		 (setf (game-screen-buffer game)
		       (sdl:create-surface 240 320))

		 ;; Load bullet sprite sheet
		 (setf (game-bullet-images game)
		       (sdl-image:load-image 
			"Resources/Sprites/shots.gif" 
			:image-type :GIF :force t :color-key-at #(0 0)))
		 ;; Create cells for bullet sprite sheet
		 (setf (game-bullet-images-cells game)
		       (loop for y from 0 to (- 192 16) by 16
			  append (loop for x from 0 to (- 192 16) by 16
				    collect (list x y 16 16))))
		 ;; Assign cells to bullet sprite sheet  
		 (setf (sdl:cells (game-bullet-images game))
		       (game-bullet-images-cells game))
		 
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
  (width (play-area game)))

(defmethod height ((game game))
  (height (play-area game)))


;;; Mutators
(defmethod add-playerf ((game game) (player player))
  (vector-push-extend player (players game)))

(defmethod add-enemyf ((game game) (enemy enemy))
  (vector-push-extend enemy (enemies game)))

(defmethod add-enemy-shotf ((game game) (enemy-shot game-object))
  (vector-push-extend enemy-shot (enemy-shots game)))

(defmethod add-player-shotf ((game game) (player-shot game-object))
  (vector-push-extend player-shot (player-shots game)))

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
  (sdl:clear-display sdl:*black* :surface (game-screen-buffer game))
  (let ((pt (sdl:point)))
    (loop for i across (enemy-shots game) 
       do
	 (setf (aref pt 0) (- (floor (x i)) 8))
	 (setf (aref pt 1) (- (floor (y i)) 8))
	 (sdl:draw-surface-at (game-bullet-images game) pt 
			      :cell (+ 24 (random 5))
			      :surface (game-screen-buffer game)))
    (loop for i across (player-shots game) 
       do
	 (setf (aref pt 0) (- (floor (x i)) 8))
	 (setf (aref pt 1) (- (floor (y i)) 8))
	 (sdl:draw-surface-at (game-bullet-images game) 
			      pt 
			      :cell 102 
			      :surface (game-screen-buffer game)))
    (loop for p across (players game)
       do
	 (sdl:draw-surface-at-* (game-player-image game)
				(- (floor (x p)) 16)
				(- (floor (y p)) 16)
				:surface (game-screen-buffer game))))

  (sdl:draw-surface-at-* (game-screen-buffer game) 0 0))


(defmethod print-debug ((game game))
  "Debug method called during keypress"
  (when (sdl:get-key-state :sdl-key-f1)
    (format t "player shots: ~a~%" (length (player-shots game)))
    (format t "enemy shots: ~a~%" (length (enemy-shots game)))
    (format t "enemies: ~a~%" (length (enemies game)))
    (format t "players: ~a~%" (length (players game)))))

(defmethod reset-gamef ((game game))
  "Reset the game to default state"
  (setf (game-state game) 'title)
  (reset-fill-vectorf (player-shots game))
  (reset-fill-vectorf (enemy-shots game))
  (reset-fill-vectorf (enemies game)))

(defmethod reset-fill-vectorf ((vec vector))
  "Set the fill pointer to 0, prepare to stomp over old values"
  (setf (fill-pointer vec) 0))

(defmethod stepf ((game game))
  "Main step function for the game"
  (print-debug game)
  (with-accessors ((players players)
		   (player-shots player-shots)
		   (enemies enemies)
		   (enemy-shots enemy-shots)
		   (play-area play-area))
      game
    (burst-fire (* (width game) .5)
		(* (height game) .5)
		1
		(random (* 2 PI))
		1
		(lambda (x y direction) 
		  (burst-newf game x y direction)))

    (loop for i across players 
       do (unless (dead i)
	    (updatef i)))
    (loop for i across player-shots 
       do
	 (when (not (collidep play-area (get-hitbox i)))
	   (setf (dead i) t))
	 (unless (dead i)
	   (stepf i)))
    (loop for i across enemies 
       do
	 (when (not(collidep play-area (get-hitbox i)))
	   (setf (dead i) t))
	 (unless (dead i)
	   (stepf i)))
    (loop for i across enemy-shots 
       do
	 (when (not (collidep play-area (get-hitbox i)))
	   (setf (dead i) t))
	 (unless (dead i)
	   (stepf i)))
    
    ;; game objection interactions here


    ;; cull dead shots and enemies
    (setf enemies (delete-if #'dead enemies))
    (setf enemy-shots (delete-if #'dead enemy-shots))
    (setf player-shots (delete-if #'dead player-shots)))
  (draw-gamef game))

