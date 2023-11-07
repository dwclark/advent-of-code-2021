(defpackage :day-02 (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:export #:exec))
(in-package :day-02)

(defun part-1 (commands)
  (loop for (sym by) in commands
	sum (if (eq sym 'forward) by 0) into horizontal
	sum (if (eq sym 'down) by (if (eq sym 'up) (- by) 0)) into depth
	finally (return (* horizontal depth))))

(defun part-2 (commands)
  (loop for (sym by) in commands
	sum (if (eq sym 'down) by (if (eq sym 'up) (- by) 0)) into aim
	sum (if (eq sym 'forward) by 0) into horizontal
	sum (if (eq sym 'forward) (* aim by) 0) into depth
	finally (return (* horizontal depth))))

(defun exec ()
  (labels ((parse (line) (read-from-string (concatenate 'string "(" line ")"))))
    (let ((commands (mapcar #'parse (read-day-file "02"))))
      (print-assert "Part 1:" (part-1 commands) 1989014)
      (print-assert "Part 2:" (part-2 commands) 2006917119))))
