;;;; shmup-test.lisp

(in-package #:shmup-test)

;;; "shmup-test" goes here. Hacks and glory await!


(defparameter *width* 240)
(defparameter *height* 320)


;; This function sets the base path for loading resources
;; Thanks to Pavel Korolev (borodust)
(defvar *assets-path* (asdf:system-relative-pathname :shmup-test #p"Resources/Sprites/"))


(defun path+file (file)
(namestring 
 (merge-pathnames
  file *assets-path*)))


(defun shmup-test ()
  ;; A single instance of game starts the whole program
  (make-instance 'game :width *width* :height *height*))
