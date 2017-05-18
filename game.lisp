(in-package #:shmup-test)
;;;; This is the game class, that actually handles resources and major
;;;; state changes withing the game


;; Accessory defuns
(defun intrpl (p a b)
  "Return the value at p(0.0 - 1.0) between a and b"
  (+ a (* (- b a) p)))

(defun burst-fire (&key x y num-shots aim spread func)
  "burst-fire will pass the value x, y, direction 
recipient-function,"
  (if (< num-shots 1) (return-from burst-fire))
  (when (= num-shots 1) ; 1 or less shots
    (funcall func x y aim)
    (return-from burst-fire))
  (let ((start-angle (- aim (* spread .5)))
	(step-angle (/ spread (1- (* 1.0 num-shots)))))
    (dotimes (i num-shots)
      (funcall func x y (+ start-angle (* step-angle i))))))

(defun interpolate-burst-fire (p &key x  y  num-shots  aim  spread
				      x2 y2 num-shots2 aim2 spread2
				      func)
  (funcall #'burst-fire 
	   :x (intrpl p x x2)
	   :y (intrpl p y y2)
	   :num-shots (round (intrpl p num-shots num-shots2))
	   :aim (intrpl p aim aim2)
	   :spread (intrpl p spread spread2)
	   :func func))




(defclass game ()
  ((state   :accessor state :initform 'title)

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

   (play-area :accessor play-area :initform (make-hitbox 240 320 120 160)
	      :documentation "Hitbox for defining play area")

   ;; Some sprite related routines
   (screen-buffer :accessor game-screen-buffer
		  :documentation "Screen buffer slot, 240x320"
		  :initform nil)
   (player-image :accessor game-player-image)
   (bullet-images :accessor game-bullet-images)
   (bullet-images-cells :accessor game-bullet-images-cells)))

(defmethod initialize-instance :after ((game game) &key 
				       (width 600) 
				       (height 800))
  (setf (play-area game)
	(make-hitbox width height (/ width 2) (/ height 2)))

  (dotimes (i 2)
    (add-playerf game
		 (make-instance 'player 
				:x (random (width game))
				:y (random (height game))
				:hitbox (make-hitbox 8 10))))

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
	(lambda () (burst-fire :x (- (x (aref (players game) 0)) 8)
			       :y (y (aref (players game) 0))
			       :num-shots 3
			       :aim (* pi 1.5)
			       :spread (* pi .1)
			       :func (lambda (x y direction)
					   (player-shootf 
					    game 
					    x 
					    y 
					    direction)))
		   (burst-fire :x (+ (x (aref (players game) 0)) 8)
			       :y (y (aref (players game) 0))
			       :num-shots 3
			       :aim (* pi 1.5)
			       :spread (* pi .1)
			       :func (lambda (x y direction)
					   (player-shootf 
					    game 
					    x 
					    y 
					    direction)))))

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
	(lambda () (burst-fire :x (x (aref (players game) 1))
			       :y (y (aref (players game) 1))
			       :num-shots 1
			       :aim (* pi 1.5)
			       :spread (* pi .1)
			       :func (lambda (x y direction)
				       (player-shootf game x y direction)))))


  ;; Should always be the last function executed in init as it starts SDL
  (sdl:load-library) ;; DO NOT APPEND BELOW THIS FORM
  (sdl:with-init () 
    (sdl:window  240 320 :title-caption "Shmup Test" :DOUBLE-BUFFER T)
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
    ;; Test case code
    (testing-setup game)
    ;; End test case code
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (stepf game)
	     (sdl:update-display)))))


(defmethod testing-setup ((game game))
  ;; Dummy function to set up test scenarios
  (let ((e (make-instance 'enemy :x 120 :y 64))
	(em (make-instance 'emitter :repeating t)))
    (setf (hitbox e) (make-hitbox 32 32 (x e) (y e)))
    (setf (speed e) .5)
    (setf (direction e) (* PI .5))
    (add-enemyf game e))

)


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
		   (make-game-object (floor x)
				     (floor y)
				     :speed 5.0
				     :direction direction
				     :width 16
				     :height 16)))

(defmethod player-shootf ((game game) x y direction)
  (add-player-shotf game
		    (make-game-object x
				      y
				      :speed 14.0
				      :direction direction
				      :width 16
				      :height 16)))

(defmethod draw-hitbox ((game game) (hitbox hitbox))
  (with-accessors ((w width) 
		   (h height) 
		   (cen-x center-x) 
		   (cen-y center-y))
                  hitbox
      (sdl:draw-rectangle-*  (round (- cen-x (/ w 2.0)))
			     (round (- cen-y (/ h 2.0))) 
			     w
			     h
			     :surface (game-screen-buffer game)
			     :color sdl:*green*)))

(defmethod draw-gamef ((game game))
  "Main method for drawing game"
  (sdl:clear-display sdl:*black* :surface (game-screen-buffer game))
  (let ((pt (sdl:point)))
    (loop for i across (enemy-shots game) 
       do
	 (setf (aref pt 0) (- (round (x i)) 8))
	 (setf (aref pt 1) (- (round (y i)) 8))
	 (sdl:draw-surface-at (game-bullet-images game) pt 
			      :cell 6
			      :surface (game-screen-buffer game))
	 (draw-hitbox game (hitbox i)))
    (loop for i across (player-shots game) 
       do
	 (setf (aref pt 0) (- (round (x i)) 8))
	 (setf (aref pt 1) (- (round (y i)) 8))
	 (sdl:draw-surface-at (game-bullet-images game) 
			      pt 
			      :cell 1
			      :surface (game-screen-buffer game)))
    (loop for p across (players game)
       do
	 (sdl:draw-surface-at-* (game-player-image game)
				(- (round (x p)) 16)
				(- (round (y p)) 16)
				:surface (game-screen-buffer game))
	 (draw-hitbox game (hitbox p)))

    (loop for e across (enemies game)
       do
	 (draw-hitbox game (hitbox e))))



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
  (setf (state game) 'title)
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
    (loop for i across players 
       do (unless (dead i)
	    (updatef i)))
    (loop for i across player-shots 
       do
	 (when (not (collidep play-area (hitbox i)))
	   (setf (dead i) t))
	 (unless (dead i)
	   (stepf i)))
    (loop for i across enemies 
       do
	 (when (not(collidep play-area (hitbox i)))
	   (setf (dead i) t))
	 (unless (dead i)
	   (stepf i)))
    (loop for i across enemy-shots 
       do
	 (when (not (collidep play-area (hitbox i)))
	   (setf (dead i) t))

	 (loop for p across players 
	    do
	      (and (collidep (hitbox i) (hitbox p))
		(setf (dead i) t)))

	 (unless (dead i)
	   (stepf i)))
    
    ;; game objection interactions here


    ;; cull dead shots and enemies
    (setf enemies (delete-if #'dead enemies))
    (setf enemy-shots (delete-if #'dead enemy-shots))
    (setf player-shots (delete-if #'dead player-shots)))
  (draw-gamef game))

