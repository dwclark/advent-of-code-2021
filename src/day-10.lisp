(defpackage :day-10
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :split)
  (:import-from :alexandria :curry :map-permutations :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-10)

(defparameter *opening* "([{<")
(defparameter *closing* ")]}>")

(defun opening-p (c) (position c *opening* :test #'string=))
(defun closing-p (c) (position c *closing* :test #'string=))
(defun match-p (c1 c2) (= (opening-p c1) (closing-p c2)))

(defun illegal-score (c)
  (cond ((string= c ")") 3)
        ((string= c "]") 57)
        ((string= c "}") 1197)
        ((string= c ">") 25137)))

(defun incomplete-score (c)
  (cond ((string= c "(") 1)
        ((string= c "[") 2)
        ((string= c "{") 3)
        ((string= c "<") 4)))

(defun answers ()
    (let ((lines (read-day-file "10"))
        (illegal-errors (make-array 0 :adjustable t :fill-pointer 0))
        (incompletes (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for line in lines
          with code-stack = nil
          do (loop with was-illegal = nil
                   for s across line
                   do (if (opening-p s)
                          (push s code-stack)
                          (let ((prev (pop code-stack)))
                            (if (not (match-p prev s))
                                (progn
                                  (setf was-illegal t)
                                  (vector-push-extend s illegal-errors)))))
                   finally (progn
                             (if (not was-illegal)
                                 (let ((score 0))
                                   (dolist (n code-stack)
                                     (setf score (* score 5))
                                     (incf score (incomplete-score n)))
                                   (vector-push-extend score incompletes)))
                             (setf code-stack nil))))
    (values (reduce #'+ illegal-errors :key #'illegal-score)
            (aref (sort incompletes #'<) (floor (/ (length incompletes) 2))))))

(defun part-1 ()
  (multiple-value-bind (p1 p2) (answers)
    (declare (ignore p2))
    p1))

(defun part-2 ()
  (multiple-value-bind (p1 p2) (answers)
    (declare (ignore p1))
    p2))
