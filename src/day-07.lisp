(defpackage :day-07
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :alist-hash-table :hash-table-values)
  (:export #:part-1 #:part-2))

(in-package :day-07)

(defun initial-list ()
  (let ((s (car (read-day-file "07"))))
    (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")"))))

(defun compute-total-cost (cost-func)
  (let* ((lst (initial-list))
         (table (alist-hash-table (mapcar #'(lambda (c) (cons c 0)) lst))))
    (loop for proposed from (apply #'min lst) to (apply #'max lst)
          do (setf (gethash proposed table)
                   (reduce #'+ (mapcar #'(lambda (n) (funcall cost-func (abs (- n proposed)))) lst))))
    (car (sort (hash-table-values table) #'<))))

(defun part-1 ()
  (compute-total-cost #'identity))

(defun part-2 ()
  (flet ((func (diff) (* (/ diff 2) (+ 1 diff))))
    (compute-total-cost #'func)))
