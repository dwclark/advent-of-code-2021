(defpackage :utils
  (:use :cl)
  (:export #:read-file :read-day-file :split-blank-lines #:*input-directory* 
           #:bit-vector->integer #:integer->bit-vector #:power-set))

(in-package :utils)

(defparameter *input-directory* nil)

(defun read-file (file-name)
  (with-open-file (stm file-name)
    (loop for line = (read-line stm nil)
          while line
          collect line)))

(defun read-day-file (day)
  (read-file (concatenate 'string *input-directory* "day-" day ".txt")))

(defun load-numbers (day)
  (map 'vector #'parse-integer (read-day-file day)))

(defun power-set (arg-list)
  (if (null arg-list)
      (list nil)
      (let ((prev (power-set (rest arg-list))))
        (append (mapcar (lambda (e)
                          (append (list (first arg-list)) e))
                        prev)
                prev))))
                        
(defun split-blank-lines (lines)
  (let ((list-of-lists nil)
        (current nil))
    (loop for line in lines
          do (if (= 0 (length line))
                 (progn
                   (push (nreverse current) list-of-lists)
                   (setf current nil))
                 (push line current))
          finally (return (progn
                            (push (nreverse current) list-of-lists)
                            (nreverse list-of-lists))))))

(defun bit-vector->integer (bit-vector)
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (width arg)
  (loop for i from (1- width) downto 0
        with bitvec = (make-array width :element-type 'bit)
        with num = arg
        do (progn
             (setf (aref bitvec i) (rem num 2))
             (setf num (floor (/ num 2))))
        finally (return bitvec)))
