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

(defun read-rounds (s)
  (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")")))

(defun read-bingo-cards (lst)
  (flet ((to-cons (tmp) (mapcar #'(lambda (e) (cons e nil)) tmp)))
    (mapcar #'(lambda (sub)
                (make-array (* *side* *side*) :initial-contents
                            (to-cons (read-from-string (concatenate 'string "(" sub ")")))))
            lst)))

(defun bingo-round (num bingo-cards)
  (loop for bingo-card in bingo-cards
        do (loop for cell across bingo-card
                 do (if (= num (car cell))
                        (setf (cdr cell) t)))
        finally (return bingo-cards)))

(defun sum-unmarked (bingo-card)
  (reduce #'+ bingo-card :key #'(lambda (cell) (if (not (cdr cell)) (car cell) 0))))

(defun winner-p (bingo-card)
  (find-if (lambda (combo)
             (= *side* (reduce #'+ combo :key #'(lambda (idx) (if (cdr (aref bingo-card idx)) 1 0)))))
           *combos*))

(defun part-1 ()
  (let* ((entries (read-blank-line-blocks "04"))
         (rounds (read-rounds (car entries)))
         (bingo-cards (read-bingo-cards (rest entries))))
    (loop for num in rounds
          do (progn
               (bingo-round num bingo-cards)
               (let ((winner (first (remove-if-not #'winner-p bingo-cards))))
                 (if winner (return-from part-1 (* num (sum-unmarked winner)))))))))

(defun part-2 ()
  (let* ((entries (read-blank-line-blocks "04"))
         (rounds (read-rounds (car entries)))
         (bingo-cards (read-bingo-cards (rest entries))))
    (loop with remaining = bingo-cards
          for num in rounds
          do (progn
               (bingo-round num remaining)
               (let ((winner (first (remove-if-not #'winner-p remaining))))
                 (if winner
                     (setf remaining (remove-if #'winner-p remaining)))
                 (if (not remaining)
                     (return-from part-2 (* num (sum-unmarked winner)))))))))
