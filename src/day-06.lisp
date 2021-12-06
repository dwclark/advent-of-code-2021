(defpackage :day-06
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-06)

(defun initial-array ()
  (let* ((s (car (read-day-file "06")))
         (lst (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")")))
         (ary (make-array 9 :initial-element 0)))
    (dolist (e lst)
      (incf (aref ary e)))
    ary))
    
(defun fish-day (ary)
  (let ((spawning (aref ary 0)))
    (rotatef (aref ary 0) (aref ary 1) (aref ary 2) (aref ary 3)
             (aref ary 4) (aref ary 5) (aref ary 6) (aref ary 7)
             (aref ary 8))
    (incf (aref ary 6) spawning)))

(defun fish-days (days)
  (let ((ary (initial-array)))
    (dotimes (n days)
      (fish-day ary))
    (reduce #'+ ary)))

(defun part-1 ()
  (fish-days 80))

(defun part-2 ()
  (fish-days 256))

