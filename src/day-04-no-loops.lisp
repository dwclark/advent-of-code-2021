(defpackage :day-04-no-loops
  (:use :cl)
  (:import-from :utils :read-blank-line-blocks)
  (:import-from :alexandria :iota :curry :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-04-no-loops)

(defparameter *side* 5)
(defparameter *0..<5* (iota 5))

(defun compute-position (p1 p2) (+ (* *side* p1) p2))

(defparameter *winning-rows* 
  (mapcar #'(lambda (r) (mapcar (curry #'compute-position r) *0..<5*)) *0..<5*))

(defparameter *winning-columns*
  (mapcar #'(lambda (r) (mapcar (rcurry #'compute-position r) *0..<5*)) *0..<5*))
                          
(defparameter *combos* (concatenate 'list *winning-rows* *winning-columns*))

(defun read-rounds (s)
  (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")")))

(defun read-bingo-cards (lst)
  (flet ((to-cons (tmp) (mapcar #'(lambda (e) (cons e nil)) tmp)))
    (mapcar #'(lambda (sub)
                (make-array (* *side* *side*) :initial-contents
                            (to-cons (read-from-string (concatenate 'string "(" sub ")")))))
            lst)))

(defun bingo-round (num bingo-cards)
  (dolist (bingo-card bingo-cards)
    (let ((found (find-if (curry #'= num) bingo-card :key #'car)))
      (if found (setf (cdr found) t))))
  bingo-cards)

(defun sum-unmarked (bingo-card)
  (reduce #'+ bingo-card :key #'(lambda (cell) (if (not (cdr cell)) (car cell) 0))))

(defun winner-p (bingo-card)
  (find-if (lambda (combo)
             (= *side* (reduce #'+ combo :key #'(lambda (idx) (if (cdr (aref bingo-card idx)) 1 0)))))
           *combos*))

(defun part-1 ()
  (let* ((entries (read-blank-line-blocks "04"))
         (rounds (read-rounds (first entries)))
         (bingo-cards (read-bingo-cards (rest entries))))
    (dolist (num rounds)
      (let ((winner (find-if #'winner-p (bingo-round num bingo-cards))))
        (if winner (return-from part-1 (* num (sum-unmarked winner))))))))

(defun part-2 ()
  (let* ((entries (read-blank-line-blocks "04"))
         (rounds (read-rounds (first entries)))
         (bingo-cards (read-bingo-cards (rest entries)))
         (remaining bingo-cards))
    (dolist (num rounds)
      (let ((winner (find-if #'winner-p (bingo-round num remaining))))
        (if winner
            (setf remaining (remove-if #'winner-p remaining)))
        (if (not remaining)
            (return-from part-2 (* num (sum-unmarked winner))))))))
