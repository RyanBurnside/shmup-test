;;;; shmup-test.asd

(asdf:defsystem #:shmup-test
  :description "A simple test to see how viable Lisp is for a 2D shmup"
  :author "Ryan Burnside"
  :license "GPL whatever"
  :serial t
  :depends-on (#:cl-opengl #:lispbuilder-sdl #:clinch)
  :components (
	       (:file "package")
	       (:file "ticker")
	       (:file "hitbox")
	       (:file "mover")
	       (:file "game-object")
	       (:file "generic-functions")
               (:file "shmup-test")))


