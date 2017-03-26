;;;; shmup-test.lisp

(in-package #:shmup-test)

;;; "shmup-test" goes here. Hacks and glory await!


(defparameter *width* 600)
(defparameter *height* 800)
(defparameter *center-width* (/ *width*  2))
(defparameter *center-height* (/ *height*  2))
(defparameter *objects* '())
(defparameter *screen-hitbox* 
  (make-instance 'hitbox 
		 :center-x *center-width*
		 :center-y *center-height*
		 :width *center-width*
		 :height *center-height*))

(defun new-rand-object ()
  (make-instance 'game-object
		 :hitbox (make-instance 'hitbox
					:center-x *center-width*
					:center-y *center-height*
					:width 32
					:height 32)
		 :x *center-width*
		 :y *center-height*
		 :speed (+ (* (random 1.0) 5.0) 1.0)
		 :direction (* (random 2.0) pi)))
							   
(defun update-objects ()
  (loop for i in *objects* do
    (stepf i)
    (when (not (collidep *screen-hitbox* (game-object-hitbox i)))
      (setf (game-object-dead i) t))))

(defun prune-dead ()
  (setf *objects* (delete-if #'game-object-dead *objects*)))

(defun shmup-test ()
  ;; Make sure the libraries are loaded on startup.
  ;; Necessary when creating a stand-alone executable.
  (sdl:load-library)
  (setf *objects* '())
  (sdl:with-init ()
    (sdl:window 
     *width* 
     *height* 
     :title-caption "Shmup Test")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (dotimes (i 4)
	       (setf *objects* (nconc *objects* 
				      (list (new-rand-object)))))
	     (update-objects)
	     (prune-dead)
	     (dolist (i *objects*)
	       (sdl:draw-filled-circle-*
			      (round (mover-x i)) 
			      (round (mover-y i)) 
			      8

			     :color sdl:*white*))

    (sdl:update-display)))))
