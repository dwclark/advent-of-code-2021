(defpackage :day-02
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-02)

(defun eval-file (func)
  (dolist (line (read-day-file "02"))
    (let ((lst (read-from-string (concatenate 'string "(" line ")"))))
      (apply func lst))))

(defun part-1 ()
  (let ((horizontal 0)
        (depth 0))
    (eval-file #'(lambda (sym x)
                   (case sym
                     (forward (incf horizontal x))
                     (down (incf depth x))
                     (up (decf depth x)))))
    (* horizontal depth)))
      

(defun part-2 ()
  (let ((horizontal 0)
        (depth 0)
        (aim 0))
    (eval-file #'(lambda (sym x)
                   (case sym
                     (down (incf aim x))
                     (up (decf aim x))
                     (forward
                      (incf horizontal x)
                      (incf depth (* aim x))))))
    (* horizontal depth)))
