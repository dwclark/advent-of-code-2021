(defpackage :day-05
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :hash-table-values :hash-table-keys)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-05)

(defun horizontal (line idx) (car (aref line idx)))
(defun vertical (line idx) (cdr (aref line idx)))

(defun horizontal-p (line)
  (= (vertical line 0) (vertical line 1)))

(defun vertical-p (line)
  (= (horizontal line 0) (horizontal line 1)))

(defun diagonal-p (line)
  (= (abs (- (horizontal line 0) (horizontal line 1)))
     (abs (- (vertical line 0) (vertical line 1)))))

(defun diagonal-points (line)
  (let ((h-func (if (< (horizontal line 0) (horizontal line 1)) #'1+ #'1-))
        (v-func (if (< (vertical line 0) (vertical line 1)) #'1+ #'1-))
        (result nil)
        (current (aref line 0)))
    (loop while (not (equal current (aref line 1)))
          do (progn
               (push current result)
               (setf current (cons (funcall h-func (car current))
                                   (funcall v-func (cdr current)))))
          finally (push current result))
    result))

(defun parse-line (line)
  (do-register-groups ((#'parse-integer i1 i2 i3 i4)) ("^(\\d+),(\\d+) -> (\\d+),(\\d+)$" line)
    (return (vector (cons i1 i2) (cons i3 i4)))))

(defun parse-lines ()
  (mapcar #'parse-line (read-day-file "05")))

(defun add-horizontals (lines table)
  (dolist (line lines)
    (if (vertical-p line)
        (loop with h = (horizontal line 0)
              for v from (min (vertical line 0) (vertical line 1)) to (max (vertical line 0) (vertical line 1))
              do (incf (gethash (cons h v) table 0)))))
  table)

(defun add-verticals (lines table)
  (dolist (line lines)
    (if (horizontal-p line)
        (loop with v = (vertical line 0)
              for h from (min (horizontal line 0) (horizontal line 1)) to (max (horizontal line 0) (horizontal line 1))
              do (incf (gethash (cons h v) table 0)))))
  table)

(defun add-diagonals (lines table)
  (dolist (line lines)
    (if (diagonal-p line)
        (dolist (key (diagonal-points line))
          (incf (gethash key table 0))))))
        
(defun part-1 ()
  (let* ((lines (parse-lines))
         (table (make-hash-table :test #'equal)))
    (add-horizontals lines table)
    (add-verticals lines table)
    (count-if #'(lambda (val) (< 1 val)) (hash-table-values table))))

(defun part-2 ()
  (let* ((lines (parse-lines))
         (table (make-hash-table :test #'equal)))
    (add-horizontals lines table)
    (add-verticals lines table)
    (add-diagonals lines table)
    (count-if #'(lambda (val) (< 1 val)) (hash-table-values table))))
