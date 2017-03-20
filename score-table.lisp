;;; The score table is a container for score-entry structs
;;; You should not manually adjust the data within use the interfaces
;;; The score table adds and stores scores in a stable sorted read only list

(in-package #:shmup-test)

(defstruct score-entry
  (name "" :type string)
  (score 0 :type integer))

(defclass score-table ()
  ((scores :reader score-table-scores :initform '())
   (length :reader score-table-length :initarg :length :initform 10)))

(defun %interpolate (a b percent)
  "A function to return a value between a and b with percent [0 to 1.0]"
  (let ((delta (- b a)))
    (+ a (* percent delta))))

(defmethod resetf ((table score-table) 
		   (fill-name string) 
		   (min-val integer)
		   (max-val integer))
  "Reset the score table, interpolate over min-val max-val with filler name"
  (let ((min-v (min min-val max-val))
	(max-v (max min-val max-val)))
    (setf (slot-value table 'scores)
	  (loop 
	    for i 
	    from (1- (score-table-length table))
	    downto 0
	    collecting 
	    (make-score-entry 
	     :name fill-name
	     :score 
	     (round (%interpolate min-v
				  max-v
				  (/ (* i 1.0) 
				     (1- (score-table-length table))))))))))

(defmethod initialize-instance :after ((object score-table) &key)
  "Create a score table, fill the list with dummy data"
  (with-slots (scores length) object
      (setf scores (loop for i 
			 from 0
			 to (1- length)
			 collecting (make-score-entry)))))

(defmethod add-entryf ((object score-table) (name string) (score number))
  "Add score entry to table IF it is higher than last value"
  (with-slots (scores length) object
    (let ((new-score (make-score-entry :name name :score score))
	  (compare #'(lambda (a b) (> (score-entry-score a)
				    (score-entry-score b)))))
      (setf scores (append scores `(,new-score)))
      (setf scores (stable-sort scores compare))
      (setf scores (butlast scores)))))

(defmethod score-high-enoughp ((object score-table) (score integer))
  "A predicate to see if the player score is high enough for the table"
  (> score (score-entry-score (car (last (score-table-scores object))))))

