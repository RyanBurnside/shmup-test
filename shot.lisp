

(in-package #:shmup-test)

(defclass shot (game-object)
  ((visual-tag :accessor visual-tag 
	       :initarg :visual-tag
	       :documentation "A slot to let the game attach graphics meaning")))

(defun make-shot (visual-tag &key x y speed direction hitbox)
  (make-instance 'shot 
		 :visual-tag visual-tag
		 :x x 
		 :y y 
		 :speed speed 
		 :direction direction
		 :hitbox hitbox))
