(defpackage :day-16
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :hash-table-alist :ensure-gethash)
  (:export #:part-1 #:part-2))

(in-package :day-16)

(defstruct packet version type-id)
(defstruct (literal (:include packet)) value)
(defstruct (operator (:include packet)) sub-packets)

(defun parse-version (bytes start-index)
  (let ((next-index start-index))
    (values (ldb (byte 3 (decf next-index 3)) bytes) next-index)))

(defun parse-type-id (bytes start-index)
  (let ((next-index start-index))
    (values (ldb (byte 3 (decf next-index 3)) bytes) next-index)))

(defun parse-literal-packet (version type-id bytes start-index)
  (let ((accum 0))
    (loop for index from (- start-index 5) downto 0 by 5
          do (progn
               (let ((next-val (ldb (byte 4 index) bytes)))
                 (setf accum (ash accum 4))
                 (setf accum (logior accum next-val)))
               (if (= 0 (ldb (byte 1 (+ 4 index)) bytes))
                   (return-from parse-literal-packet
                     (values (make-literal :version version :type-id type-id :value accum)
                             index)))))))

(defun parse-operator-packet (version type-id bytes start-index)
  (let* ((next-index start-index)
         (length-type-id (ldb (byte 1 (decf next-index)) bytes))
         (sub-packets nil))
    (case length-type-id
      (0 (let* ((bit-length (ldb (byte 15 (decf next-index 15)) bytes)))
           (setf sub-packets (loop with terminate = (- next-index bit-length)
                                   while (not (= next-index terminate))
                                   collecting (let ((p nil))
                                                (setf (values p next-index) (parse-packet bytes next-index))
                                                p)))))
       (1 (let* ((num-packets (ldb (byte 11 (decf next-index 11)) bytes)))
            (setf sub-packets (loop for i from 0 below num-packets
                                    collecting (let ((p nil))
                                                 (setf (values p next-index) (parse-packet bytes next-index))
                                                 p))))))
    (values (make-operator :version version :type-id type-id :sub-packets sub-packets)
            next-index)))

(defun parse-packet (bytes start-index)
  (let ((next-index start-index)
        (version 0)
        (type-id 0))
    (setf (values version next-index) (parse-version bytes next-index))
    (setf (values type-id next-index) (parse-type-id bytes next-index))
    (case type-id
      (4 (parse-literal-packet version type-id bytes next-index))
      (otherwise (parse-operator-packet version type-id bytes next-index)))))

(defun start-parse (str)
  (let ((num-bits (* 4 (length str)))
        (num (parse-integer str :radix 16)))
    (parse-packet num num-bits)))

(defun sum-versions (packet)
  (+ (packet-version packet)
     (if (literal-p packet)
         0
         (reduce #'+ (operator-sub-packets packet) :key #'sum-versions))))

(defun part-1 ()
  (let* ((str (car (read-day-file "16")))
         (packet (start-parse str)))
    (sum-versions packet)))

(defun calculate-values (packet)
  (case (packet-type-id packet)
    (0 (reduce #'+ (operator-sub-packets packet) :key #'calculate-values))
    (1 (reduce #'* (operator-sub-packets packet) :key #'calculate-values :initial-value 1))
    (2 (reduce #'min (operator-sub-packets packet) :key #'calculate-values :initial-value most-positive-fixnum))
    (3 (reduce #'max (operator-sub-packets packet) :key #'calculate-values :initial-value most-negative-fixnum))
    (4 (literal-value packet))
    (5 (let ((lst (operator-sub-packets packet)))
         (if (> (calculate-values (first lst)) (calculate-values (second lst))) 1 0)))
    (6 (let ((lst (operator-sub-packets packet)))
         (if (< (calculate-values (first lst)) (calculate-values (second lst))) 1 0)))
    (7 (let ((lst (operator-sub-packets packet)))
         (if (= (calculate-values (first lst)) (calculate-values (second lst))) 1 0)))))
    
(defun part-2 ()
  (let* ((str (car (read-day-file "16")))
         (packet (start-parse str)))
    (calculate-values packet)))
