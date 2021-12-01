(defpackage :day-01
  (:use :cl)
  (:import-from :utils :load-numbers)
  (:export #:part-1 #:part-2))

(in-package :day-01)

(defparameter *nums* (concatenate 'list (load-numbers "01")))

(defun count-prev< (lst)
  (count-if #'(lambda (c) (< (car c) (cdr c)))
            (mapcar #'cons lst (rest lst))))

(defun part-1 ()
  (count-prev< *nums*))

(defun part-2 ()
  (count-prev< (mapcar #'(lambda (lst) (apply #'+ lst))
                       (mapcar 'list *nums* (rest *nums*) (rest (rest *nums*))))))
