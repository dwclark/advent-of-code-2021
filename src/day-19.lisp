(defpackage :day-19
  (:use :cl)
  (:import-from :utils :read-blank-line-blocks)
  (:import-from :cl-ppcre :split :do-register-groups)
  (:import-from :alexandria :hash-table-alist)
  (:export #:part-1 #:part-2))

(in-package :day-19)

(defun get-x (coord)
  (aref coord 0))

(defun get-y (coord)
  (aref coord 1))

(defun get-z (coord)
  (aref coord 2))

(defun roll (coord)
  (vector (get-x coord) (get-z coord) (- (get-y coord))))

(defun turn (coord)
  (vector (- (get-y coord)) (get-x coord) (get-z coord)))

(defun all-transforms (coord)
  (loop with v = coord
        with all = nil
        for cycle from 0 to 1
        do (loop for step from 0 to 2
                 do (progn
                      (setf v (roll v))
                      (push v all)
                      (loop for i from 0 to 2
                            do (progn
                                 (setf v (turn v))
                                 (push v all))))
                 finally (setf v (roll (turn (roll v)))))
        finally (return all)))

(defparameter *all-transforms* 
  (list #'(lambda (c) (vector (- (get-z c)) (- (get-y c)) (- (get-x c))))
        #'(lambda (c) (vector (- (get-y c)) (get-z c) (- (get-x c))))
        #'(lambda (c) (vector (get-z c) (get-y c) (- (get-x c))))
        #'(lambda (c) (vector (get-y c) (- (get-z c)) (- (get-x c))))
        #'(lambda (c) (vector (get-y c) (get-x c) (- (get-z c))))
        #'(lambda (c) (vector (get-x c) (- (get-y c)) (- (get-z c))))
        #'(lambda (c) (vector (- (get-y c)) (- (get-x c)) (- (get-z c))))
        #'(lambda (c) (vector (- (get-x c)) (get-y c) (- (get-z c))))
        #'(lambda (c) (vector (- (get-x c)) (get-z c) (get-y c)))
        #'(lambda (c) (vector (get-z c) (get-x c) (get-y c)))
        #'(lambda (c) (vector (get-x c) (- (get-z c)) (get-y c)))
        #'(lambda (c) (vector (- (get-z c)) (- (get-x c)) (get-y c)))
        #'(lambda (c) (vector (get-x c) (get-y c) (get-z c)))
        #'(lambda (c) (vector (get-y c) (- (get-x c)) (get-z c)))
        #'(lambda (c) (vector (- (get-x c)) (- (get-y c)) (get-z c)))
        #'(lambda (c) (vector (- (get-y c)) (get-x c) (get-z c)))
        #'(lambda (c) (vector (- (get-y c)) (- (get-z c)) (get-x c)))
        #'(lambda (c) (vector (- (get-z c)) (get-y c) (get-x c)))
        #'(lambda (c) (vector (get-y c) (get-z c) (get-x c)))
        #'(lambda (c) (vector (get-z c) (- (get-y c)) (get-x c)))
        #'(lambda (c) (vector (get-z c) (- (get-x c)) (- (get-y c))))
        #'(lambda (c) (vector (- (get-x c)) (- (get-z c)) (- (get-y c))))
        #'(lambda (c) (vector (- (get-z c)) (get-x c) (- (get-y c))))
        #'(lambda (c) (vector (get-x c) (get-z c) (- (get-y c))))))

(defstruct scanner index coordinates)

(defun make-scanners ()
  (loop with ret = nil
        with strs = (read-blank-line-blocks "19")
        for str in strs
        do (let ((lines (split "\\n+" str)))
             (do-register-groups ((#'parse-integer index)) ("--- scanner (\\d+)" (first lines))
               (loop with coords = nil
                     for coord-str in (rest lines)
                     do (do-register-groups ((#'parse-integer x) (#'parse-integer y) (#'parse-integer z)) ("(-?\\d+),(-?\\d+),(-?\\d+)" coord-str)
                          (push (vector x y z) coords))
                     finally (push (make-scanner :index index :coordinates (reverse coords)) ret))))
        finally (return (reverse ret))))

;; c1 + diff = c2
(defun coordinate-translation (c1 c2)
  (vector (- (get-x c2) (get-x c1))
          (- (get-y c2) (get-y c1))
          (- (get-z c2) (get-z c1))))

(defun coordinate-diffs (coordinates-1 coordinates-2)
  (loop with table = (make-hash-table :test #'equalp)
        for c1 in coordinates-1
        do (loop for c2 in coordinates-2
                 do (incf (gethash (coordinate-translation c1 c2) table 0)))
        finally (return (sort (hash-table-alist table) #'(lambda (cons-1 cons-2) (> (cdr cons-1) (cdr cons-2)))))))

(defun find-transform-and-translation (scanner-1 scanner-2)
  (loop with coordinates-1 = (scanner-coordinates scanner-1)
        for func in *all-transforms*
        do (let* ((coordinates-2 (mapcar func (scanner-coordinates scanner-2)))
                  (diffs (coordinate-diffs coordinates-1 coordinates-2)))
             (if (<= 12 (cdr (first diffs)))
                 (progn
                   (format t "~A~%" (remove-if-not #'(lambda (c) (<= 12 (cdr c))) diffs))
                   (return-from find-transform-and-translation (values (car (first diffs)) func))))))
  (values nil nil))

(defun part-1 ()
  (let ((scanners (make-scanners)))
    (loop for outer-idx from 0 below (length scanners)
          do (loop with scanner-1 = (nth outer-idx scanners)
                   for inner-idx from (1+ outer-idx) below (length scanners)
                   do (let ((scanner-2 (nth inner-idx scanners)))
                        (multiple-value-bind (translation func) (find-transform-and-translation scanner-1 scanner-2)
                          (if translation
                              (format t "~A->~A ~A ~A~%" (scanner-index scanner-1) (scanner-index scanner-2) translation func))))))))

(defun part-2 ())
