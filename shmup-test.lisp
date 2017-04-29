;;;; shmup-test.lisp

(in-package #:shmup-test)

;;; "shmup-test" goes here. Hacks and glory await!


(defparameter *width* 240)
(defparameter *height* 320)

(defun shmup-test ()
  ;; A single instance of game starts the whole program
  (make-instance 'game :width *width* :height *height*))
