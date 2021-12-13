(defpackage :day-12
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :hash-table-alist)
  (:export #:part-1 #:part-2))

(in-package :day-12)

(defun read-caves ()
  (let ((init-list (read-day-file "12"))
        (hash-table (make-hash-table :test 'equal)))
    (dolist (line init-list)
      (do-register-groups (one two) ("^([a-zA-Z]+)-([a-zA-Z]+)$" line)
        (setf (gethash one hash-table) (cons two (gethash one hash-table nil)))
        (setf (gethash two hash-table) (cons one (gethash two hash-table nil)))))
    hash-table))

(defun small-p (s) (lower-case-p (aref s 0)))
(defun big-p (s) (upper-case-p (aref s 0)))
(defun end-p (lst) (equal "end" (car lst)))
(defun legal-p1-p (lst)
  (let ((tmp (make-hash-table :test 'equal)))
    (dolist (n lst)
      (incf (gethash n tmp 0)))
    (loop for k being the hash-keys in tmp using (hash-value v)
          do (if (and (small-p k) (not (= v 1)))
                 (return-from legal-p1-p nil)))
    t))

(defun legal-p2-p (lst)
  (let ((tmp (make-hash-table :test 'equal))
        (small-greater-than-1 0))
    (dolist (n lst)
      (incf (gethash n tmp 0)))
    (loop for k being the hash-keys in tmp using (hash-value v)
          do (cond ((and (equal "start" k) (< 1 v)) (return-from legal-p2-p nil))
                   ((and (small-p k) (< 2 v)) (return-from legal-p2-p nil))
                   ((and (small-p k) (= 2 v)) (incf small-greater-than-1))))
    (<= small-greater-than-1 1)))
             

(defun distinct-paths (hash f-legal-p)
  (let ((the-stack nil)
        (paths nil))
    
    (push (list "start") the-stack)
    (loop with path = (pop the-stack)
          while path
          do (progn
               ;(format t "~A~%" path)
               (cond ((end-p path) (push path paths))
                     ((funcall f-legal-p path)
                      (loop for destination in (gethash (car path) hash)
                            do (push (cons destination path) the-stack))))
               (setf path (pop the-stack))))
    
    paths))

(defun part-1 ()
  (length (distinct-paths (read-caves) #'legal-p1-p)))

(defun part-2 ()
  (length (distinct-paths (read-caves) #'legal-p2-p)))
