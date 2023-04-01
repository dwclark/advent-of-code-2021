(defpackage :day-07-statistics
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :mean :median :curry)
  (:export #:part-1 #:part-2))

(in-package :day-07-statistics)

(defun initial-list ()
  (let ((s (car (read-day-file "07"))))
    (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")"))))

(defun diff (v1 v2) (abs (- v1 v2)))

(defun part-1 ()
  (let* ((vals (initial-list))
         (the-median (round (median vals))))
    (reduce #'+ (mapcar (curry #'diff the-median) vals))))

(defun part-2 ()
  (let* ((vals (initial-list))
         (the-median (round (median vals)))
         (tmp (mean vals))
         (the-mean (if (< the-median tmp) (floor tmp) (ceiling tmp))))
    (flet ((cost (v)
             (let ((d (diff v the-mean)))
               (floor (/ (* d (1+ d)) 2)))))
      (reduce #'+ (mapcar #'cost vals)))))
