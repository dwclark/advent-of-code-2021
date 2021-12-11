(defpackage :day-11
  (:use :cl)
  (:import-from :utils :read-2d-world)
  (:import-from :alexandria :curry :map-permutations :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-11)

(defparameter *to-add* '((-1 . -1) (-1 . 0)  (-1 . 1)
                         (0 . -1)            (0 . 1)
                         (1 . -1)  (1 . 0)   (1 . 1)))

(defun each-neighbor (world row col func)
  (let ((max-row (array-dimension world 0))
        (max-col (array-dimension world 1)))
    (flet ((legal-p (r c)
             (and (<= 0 r) (<= 0 c)
                  (< r max-row) (< c max-col))))

      (loop for (add-row . add-col) in *to-add*
            do (let ((new-row (+ add-row row))
                     (new-col (+ add-col col)))
                 (if (legal-p new-row new-col)
                     (funcall func new-row new-col)))))))

(defun increase+1 (world)
  (loop for row from 0 below (array-dimension world 0)
        do (loop for col from 0 below (array-dimension world 1)
                 do (incf (aref world row col)))))

(defun process-flashes (world flashed)
  (loop for row from 0 below (array-dimension world 0)
        do (loop for col from 0 below (array-dimension world 1)
                 do (if (and (< 9 (aref world row col))
                             (not (gethash (cons row col) flashed)))
                        (progn
                          (each-neighbor world row col
                                         #'(lambda (r c) (incf (aref world r c))))
                          (setf (gethash (cons row col) flashed) t))))))

(defun reset-flashed (world flashed)
  (loop for (row . col) being the hash-keys in flashed
        do (setf (aref world row col) 0)))

(defun step-flashes (world)
  (loop with flashed = (make-hash-table :test #'equal)
        with before = (hash-table-count flashed)
        with after = -1
        while (not (= before after))
        do (progn
             (setf before (hash-table-count flashed))
             (process-flashes world flashed)
             (setf after (hash-table-count flashed)))
        finally (progn
                  (reset-flashed world flashed)
                  (return flashed))))

(defun part-1 ()
  (let ((world (read-2d-world "11"))
        (total 0))
    (dotimes (n 100)
      (increase+1 world)
      (incf total (hash-table-count (step-flashes world))))
    total))

(defun part-2 ()
  (loop with world = (read-2d-world "11")
        with step-count = 0
        with flashed = 0
        while (not (= flashed (* (array-dimension world 0) (array-dimension world 1))))
        do (progn
             (increase+1 world)
             (setf flashed (hash-table-count (step-flashes world)))
             (incf step-count))
        finally (return step-count)))
