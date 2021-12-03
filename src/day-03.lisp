(defpackage :day-03
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-03)

(defun one-bit-counts (width nums)
  (let ((counts (make-array width :initial-element 0)))
    (dotimes (idx width)
      (setf (aref counts idx)
            (count-if #'(lambda (n) (logbitp idx n)) nums)))
    counts))

(defun make-data (lst)
  (let ((parsed (mapcar #'(lambda (s) (read-from-string (concatenate 'string "#b" s))) lst)))
    (values parsed
            (one-bit-counts (length (car lst)) parsed))))

(defun part-1 ()
  (multiple-value-bind (nums counts) (make-data (read-day-file "03"))
    (let ((gamma 0)
          (epsilon 0))
      (loop for idx from 0 below (length counts)
            do (if (< (/ (length nums) 2) (aref counts idx))
                   (setf gamma (logior gamma (ash 1 idx)))
                   (setf epsilon (logior epsilon (ash 1 idx)))))
      (* gamma epsilon))))

(defun filter-down (first second)
  (multiple-value-bind (nums counts) (make-data (read-day-file "03"))
    (loop with leftover = nums
          with leftover-counts = counts
          with width = (1- (length counts))
          for idx from width downto 0
          do (progn
               (if (<= (/ (length leftover) 2) (aref leftover-counts idx))
                   (setf leftover (funcall first idx leftover))
                   (setf leftover (funcall second idx leftover)))
               (if (= 1 (length leftover))
                   (return (car leftover))
                   (setf leftover-counts (one-bit-counts width leftover)))))))


(defun part-2 ()
  (let ((first #'(lambda (idx nums) (remove-if-not #'(lambda (x) (logbitp idx x)) nums)))
        (second #'(lambda (idx nums) (remove-if #'(lambda (x) (logbitp idx x)) nums))))
    (* (filter-down first second) (filter-down second first))))
