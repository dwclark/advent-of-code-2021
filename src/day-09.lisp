(defpackage :day-09
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :split)
  (:import-from :alexandria :curry :map-permutations :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-09)

(defun generate-landscape ()
  (let ((init (mapcar #'(lambda (s) (map 'list #'digit-char-p s)) (read-day-file "09"))))
    (make-array (list (length init) (length (car init)))
                :initial-contents init)))

(defun neighbor-points (ary row col)
  (let ((max-row (array-dimension ary 0))
        (max-col (array-dimension ary 1))
        (ret (make-array 0 :adjustable t :fill-pointer 0)))

    (flet ((legal-p (cell)
             (and (<= 0 (car cell))
                  (< (car cell) max-row)
                  (<= 0 (cdr cell))
                  (< (cdr cell) max-col))))
      
      (loop for point in (list (cons (1- row) col) (cons (1+ row) col)
                               (cons row (1- col)) (cons row (1+ col)))
            do (if (legal-p point)
                   (vector-push-extend point ret))))
    
    ret))

(defun low-point-p (landscape row col)
  (every #'(lambda (cell)
             (< (aref landscape row col) (aref landscape (car cell) (cdr cell))))
         (neighbor-points landscape row col)))
  
(defun low-points (landscape)
  (loop with ret = (make-array 0 :adjustable t :fill-pointer 0)
        for row from 0 below (array-dimension landscape 0)
        do (loop for col from 0 below (array-dimension landscape 1)
                 do (if (low-point-p landscape row col)
                        (vector-push-extend (cons row col) ret)))
        finally (return ret)))

(defun basin-count (visited landscape row col)
  (if (find (cons row col) visited :test #'equal)
      0
      (labels ((visitable-p (point)
                 (not (= 9 (aref landscape (car point) (cdr point)))))
           
               (can-visit (points)
                 (remove-if-not #'visitable-p points)))
        (progn
          (vector-push-extend (cons row col) visited)
          (1+ (loop for cell across (can-visit (neighbor-points landscape row col))
                    summing (basin-count visited landscape (car cell) (cdr cell)) into count
                    finally (return count)))))))
                 
(defun part-1 ()
  (let* ((landscape (generate-landscape))
         (lows (low-points landscape)))
    (loop for cell across lows
          summing (1+ (aref landscape (car cell) (cdr cell))) into ret
          finally (return ret))))
  
(defun part-2 ()
  (let* ((landscape (generate-landscape))
        (basin-sizes (map 'vector #'(lambda (c)
                                      (basin-count (make-array 0 :adjustable t :fill-pointer 0) landscape (car c) (cdr c)))
                          (low-points landscape))))
    (sort basin-sizes #'>)
    (* (aref basin-sizes 0) (aref basin-sizes 1) (aref basin-sizes 2))))
