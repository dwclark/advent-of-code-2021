(defpackage :day-03
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-03)

(defparameter tmp (list "00100"
                        "11110"
                        "10110"
                        "10111"
                        "10101"
                        "01111"
                        "00111"
                        "11100"
                        "10000"
                        "11001"
                        "00010"
                        "01010"))

(defun one-bit-counts (width nums)
  (let ((counts (make-array width :initial-element 0)))
    (dotimes (idx width)
      (setf (aref counts idx)
            (count-if #'(lambda (n) (logbitp idx n)) nums)))
    counts))

(defun make-data (lst)
  (let ((parsed (mapcar #'(lambda (s) (read-from-string (concatenate 'string "#b" s))) lst)))
    (values (/ (length lst) 2)
            parsed
            (one-bit-counts (length (car lst)) parsed))))

(defun part-1 ()
  (multiple-value-bind (mid nums counts) (make-data (read-day-file "03"))
    (let ((gamma 0)
          (epsilon 0))
      (loop for count across counts
            for idx from 0 below (length counts)
            do (if (< mid count)
                   (setf gamma (logior gamma (ash 1 idx)))
                   (setf epsilon (logior epsilon (ash 1 idx)))))
      (* gamma epsilon))))

(defun filter-down (first second)
  (multiple-value-bind (mid nums counts) (make-data (read-day-file "03"))
    (loop with leftover = nums
          with leftover-counts = counts
          with leftover-mid = mid
          with width = (1- (length counts))
          for idx from width downto 0
          do (progn
               ;(format t "mid ~D counts ~D leftover ~B~%" leftover-mid leftover-counts leftover)
               (if (<= leftover-mid (aref leftover-counts idx))
                   (setf leftover (funcall first idx leftover))
                   (setf leftover (funcall second idx leftover)))
               (if (= 1 (length leftover))
                   (return (car leftover))
                   (progn
                     (setf leftover-counts (one-bit-counts width leftover))
                     (setf leftover-mid (/ (length leftover) 2))))))))

(defun part-2 ()
  (let ((first #'(lambda (idx leftover) (remove-if-not #'(lambda (x) (logbitp idx x)) leftover)))
        (second #'(lambda (idx leftover) (remove-if #'(lambda (x) (logbitp idx x)) leftover))))
    (* (filter-down first second) (filter-down second first))))
