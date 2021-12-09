(defpackage :day-08
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :split)
  (:import-from :alexandria :curry :map-permutations :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-08)

(defparameter *segments* "abcdefg")
(defparameter *led-digits* '("abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))

(defun remap-led (str mapping)
  (flet ((find-new-char (c)
           (aref mapping (position c *segments* :test #'char=))))
    (map 'string #'find-new-char str)))

(defun remap-leds (mapping)
  (normalize-strings (mapcar (rcurry #'remap-led mapping) *led-digits*)))

(defun all-possible-leds ()
  (let ((ary (make-array 5040 :fill-pointer 0)))
    (map-permutations #'(lambda (mapping)
                          (vector-push (remap-leds mapping) ary)) *segments* :length 7)
    ary))

(defparameter *all-possible-leds* (all-possible-leds))

(defun led-to-number (str &optional (led-digits *led-digits*))
  (position str led-digits :test #'string=))

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
          collecting (list :inputs (normalize-leds (subseq lst 0 10))
                           :outputs (normalize-leds (subseq lst 11))) into ret
          finally (return ret))))

(defun find-rewiring (input)
  (loop for possible-led across *all-possible-leds*
        do (if (not (set-difference input possible-led :test #'string=))
               (return-from find-rewiring possible-led))))
  
(defun part-2 ()
  (let* ((ios (inputs-outputs))
         (actual-leds (loop for io in ios
                            collecting (find-rewiring (getf io :inputs)) into valid-mappings
                            finally (return valid-mappings))))
    (loop for io in ios
          for led in actual-leds
          summing (loop for output in (getf io :outputs)
                        for multiplier = 1000 then (/ multiplier 10)
                        summing (* multiplier (led-to-number output led)) into output-sum
                        finally (return output-sum)) into final-sum
          finally (return final-sum))))
