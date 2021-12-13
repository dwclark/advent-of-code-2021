(defpackage :day-13
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :curry)
  (:export #:part-1 #:part-2))

(in-package :day-13)

(defun fold-y (y-val cell)
  (let ((diff (abs (- y-val (cdr cell)))))
    (cons (car cell) (- y-val diff))))

(defun fold-x (x-val cell)
  (let ((diff (abs (- x-val (car cell)))))
    (cons (- x-val diff) (cdr cell))))

(defun read-inputs ()
  (let* ((initial-list (read-day-file "13"))
         (split-at (position "" initial-list :test #'equal))
         (coordinate-list (subseq initial-list 0 split-at))
         (fold-list (subseq initial-list (1+ split-at)))
         (coordinates (mapcar #'(lambda (line)
                                  (do-register-groups ((#'parse-integer x y)) ("^([0-9]+),([0-9]+)$" line)
                                    (return (cons x y)))) coordinate-list))
         (folds (mapcar #'(lambda (line)
                            (do-register-groups (coord (#'parse-integer at)) ("^fold along (x|y)=([0-9]+)$" line)
                              (return (cons (if (string= "x" coord) #'fold-x #'fold-y) at)))) fold-list)))
    
    (values coordinates folds)))
  
(defun part-1 ()
  (multiple-value-bind (coordinates folds) (read-inputs)
    (destructuring-bind (func . at) (car folds)
      (length (remove-duplicates (mapcar (curry func at) coordinates) :test #'equal)))))

(defun part-2 ()
  (multiple-value-bind (coordinates folds) (read-inputs)
    (let* ((final-grid (loop with grid = coordinates
                             for (func . at) in folds
                             do (setf grid (remove-duplicates (mapcar (curry func at) grid) :test #'equal))
                             finally (return grid)))
           (width (reduce #'max final-grid :key #'car))
           (height (reduce #'max final-grid :key #'cdr)))
      (loop for y from 0 to height
            do (loop for x from 0 to width
                     do (format t "~A" (if (member (cons x y) final-grid :test #'equal) "#" " "))
                     finally (format t "~%"))))))
