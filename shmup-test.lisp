;;;; shmup-test.lisp

(in-package #:shmup-test)

;;; "shmup-test" goes here. Hacks and glory await!


(defparameter *width* 600)
(defparameter *height* 800)
(defparameter *center-width* (/ *width*  2))
(defparameter *center-height* (/ *height*  2))
(defparameter *objects* '())

(defparameter *engine* (make-instance 'game :width *width* :height *height*))


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
 
(defun burst-new (x y direction)
  (add-enemy-shotf *engine*
			(make-game-object
			 *center-width*
			 *center-height*
			 :speed 5.0
			 :direction direction
			 :width 16
			 :height 16)))

(defun game-update ()
  (tickf *direction-ticker*)
  (burst-fire *center-width* 
	      *center-height*
	      8
	      *shot-direction*
	      (* pi 2.0)
	      #'burst-new)
  (burst-fire *center-width* 
	      *center-height*
	      8
	      (- *shot-direction*)
	      (* pi 2.0)
	      #'burst-new)

  (stepf *engine*))



(defparameter *shot-direction* 0.0)
(defparameter *direction-ticker* (make-instance 'ticker-action
					       :ready-at 1
					       :on-ready
					       (lambda ()
						 (incf *shot-direction*
						       (/ PI 10.0)))))


(defun game-draw ()
  (loop for i across (game-enemy-shots *engine*) 
     do
       (sdl:draw-filled-circle-*
	(round (mover-x i)) 
	(round (mover-y i)) 
	8
	:color sdl:*white*)))

(defun shmup-test ()
  ;; Make sure the libraries are loaded on startup.
  ;; Necessary when creating a stand-alone executable.
  (sdl:load-library)
  (setf *objects* '())
  (sdl:with-init () (sdl:window 
		     (width *engine*)
		     (height *engine*)
		     :title-caption "Shmup Test")
		 (setf (sdl:frame-rate) 60)
		 (sdl:with-events ()
		   (:quit-event () t)
		   (:idle ()
			  (sdl:clear-display sdl:*black*)
			  (game-update)
			  (game-draw)
			  (sdl:update-display)))))
