(defpackage :day-08
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :split)
  (:import-from :alexandria :curry :map-permutations :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-08)

(defparameter *segments* "abcdefg")
(defparameter *display-digits* '("abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))

(defun normalize-display-digits (lst)
  (mapcar #'(lambda (s)
              (sort s #'char<)) lst))

(defun remap-display-digit (str mapping)
  (flet ((find-new-char (c)
           (aref mapping (position c *segments* :test #'char=))))
    (map 'string #'find-new-char str)))

(defun remap-display-digits (mapping)
  (normalize-display-digits (mapcar (rcurry #'remap-display-digit mapping) *display-digits*)))

(defun all-possible-digital-displays ()
  (let ((ary (make-array 5040 :fill-pointer 0)))
    (map-permutations #'(lambda (mapping)
                          (vector-push (remap-display-digits mapping) ary)) *segments* :length 7)
    ary))

(defparameter *all-possible-digital-displays* (all-possible-digital-displays))

(defun display-digit-to-number (str &optional (display-digits *display-digits*))
  (position str display-digits :test #'string=))

(defun part-1 ()
  (let ((outputs (mapcar (curry #'nthcdr 11) (mapcar (curry #'split "\\s") (read-day-file "08")))))
    (reduce #'+ (mapcar #'(lambda (lst)
                            (count-if #'(lambda (s)
                                          (let ((len (length s)))
                                            (or (= len 2) (= len 4) (= len 3) (= len 7)))) lst)) outputs))))

(defun inputs-outputs ()
  (let* ((initial-list (read-day-file "08"))
         (list-of-strings (mapcar #'(lambda (s) (split "\\s" s)) initial-list)))
    (loop for lst in list-of-strings
          collecting (list :inputs (normalize-display-digits (subseq lst 0 10))
                           :outputs (normalize-display-digits (subseq lst 11))) into ret
          finally (return ret))))

(defun find-rewiring (input)
  (loop for possible-digital-display across *all-possible-digital-displays*
        do (if (not (set-difference input possible-digital-display :test #'string=))
               (return-from find-rewiring possible-digital-display))))
  
(defun part-2 ()
  (let* ((ios (inputs-outputs))
         (actual-leds (loop for io in ios
                            collecting (find-rewiring (getf io :inputs)) into valid-mappings
                            finally (return valid-mappings))))
    (loop for io in ios
          for led in actual-leds
          summing (loop for output in (getf io :outputs)
                        for multiplier = 1000 then (/ multiplier 10)
                        summing (* multiplier (display-digit-to-number output led)) into output-sum
                        finally (return output-sum)) into final-sum
          finally (return final-sum))))
