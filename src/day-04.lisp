(defpackage :day-04
  (:use :cl)
  (:import-from :utils :read-blank-line-blocks)
  (:export #:part-1 #:part-2))

(in-package :day-04)

(defun generate-rows (side)
  (loop for r from 0 below side
        collecting (loop for c from 0 below side
                         collecting (+ (* side r) c))))

(defun generate-cols (side)
  (loop for r from 0 below side
        collecting (loop for c from 0 below side
                          collecting (+ (* side c) r))))

(defparameter *side* 5)
(defparameter *combos* (concatenate 'list (generate-rows *side*) (generate-cols *side*)))

(defun read-moves (s)
  (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")")))

(defun read-matrices (lst)
  (flet ((to-cons (tmp) (mapcar #'(lambda (e) (cons e nil)) tmp)))
    (mapcar #'(lambda (sub)
                (make-array (* *side* *side*) :initial-contents
                            (to-cons (read-from-string (concatenate 'string "(" sub ")")))))
            lst)))

(defun bingo-round (num matrices)
  (loop for matrix in matrices
        do (loop for cell across matrix
                 do (if (= num (car cell))
                        (setf (cdr cell) t)))
        finally (return matrices)))

(defun sum-unmarked (matrix)
  (reduce #'+ matrix :key #'(lambda (cell) (if (not (cdr cell)) (car cell) 0))))

(defun winner-p (matrix)
  (find-if (lambda (combo)
             (= *side* (reduce #'+ combo :key #'(lambda (idx) (if (cdr (aref matrix idx)) 1 0)))))
           *combos*))

(defun part-1 ()
  (let* ((entries (read-blank-line-blocks "04"))
         (moves (read-moves (car entries)))
         (matrices (read-matrices (rest entries))))
    (loop for num in moves
          do (progn
               (bingo-round num matrices)
               (let ((winner (first (remove-if-not #'winner-p matrices))))
                 (if winner (return-from part-1 (* num (sum-unmarked winner)))))))))

(defun part-2 ()
  (let* ((entries (read-blank-line-blocks "04"))
         (moves (read-moves (car entries)))
         (matrices (read-matrices (rest entries))))
    (loop with remaining = matrices
          for num in moves
          do (progn
               (bingo-round num remaining)
               (let ((winner (first (remove-if-not #'winner-p remaining))))
                 (if winner
                     (setf remaining (remove-if #'winner-p remaining)))
                 (if (not remaining)
                     (return-from part-2 (* num (sum-unmarked winner)))))))))
