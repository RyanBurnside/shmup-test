;;;; shmup-test.lisp

(in-package #:shmup-test)

;;; "shmup-test" goes here. Hacks and glory await!


(defparameter *width* 240)
(defparameter *height* 320)
(defun game-update ()
  (tickf *direction-ticker*)
  (burst-fire *center-width* 
	      *center-height*
	      1
	      *shot-direction*
	      (* pi 2.0)
	      #'burst-new)
  (burst-fire *center-width* 
	      *center-height*
	      1
	      (- *shot-direction*)
	      (* pi 2.0)
	      #'burst-new)
  (when (or (sdl:mouse-left-p) (sdl:get-key-state :sdl-key-z))
    (burst-fire (sdl:mouse-x)
		(sdl:mouse-y)
		1
		(* pi 1.5)
		(* pi .10)
		#'player-shoot))
  (stepf *engine*))

(defun shmup-test ()
  ;; A single instance of game starts the whole program
  (make-instance 'game :width *width* :height *height*)
 )
