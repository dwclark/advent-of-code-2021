(defpackage :day-01 (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:export #:exec))
(in-package :day-01)

(defun count-prev< (the-list)
  (loop for (prev . next) in (mapcar #'cons the-list (rest the-list))
	counting (< prev next)))

(defun window-averages (the-list)
  (labels ((avg (v1 v2 v3) (float (/ (+ v1 v2 v3) 3))))
    (mapcar #'avg the-list (rest the-list) (rest (rest the-list)))))

(defun exec ()
  (let ((all (mapcar #'parse-integer (read-day-file "01"))))
    (print-assert "Part 1:" (count-prev< all) 1692)
    (print-assert "Part 2:" (count-prev< (window-averages all)) 1724)))
