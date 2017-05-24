;;;; shmup-test.asd




(asdf:defsystem #:shmup-test
  :description "A simple test to see how viable Lisp is for a 2D shmup"
  :author "Ryan Burnside"
  :license "GPL whatever"
  :serial t
  :depends-on (#:lispbuilder-sdl 
	       #:lispbuilder-sdl-image)
  :components (
	       (:file "package")
	       (:file "score-table")
	       (:file "ticker")
	       (:file "hitbox")
	       (:file "mover")
	       (:file "game-object")
	       (:file "shot")
	       (:file "emitter")
	       (:file "enemy")
	       (:file "player")
	       (:file "game")
	       (:file "generic-functions")
               (:file "shmup-test")))
