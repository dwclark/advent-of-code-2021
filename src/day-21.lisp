(defpackage :day-21
  (:use :cl)
  (:export #:part-1 #:part-2))

(in-package :day-21)

(defvar *deterministic* 1)
(defvar *deterministic-rolls* 0)

(defun reset-deterministic ()
  (setf *deterministic* 1)
  (setf *deterministic-rolls* 0))

(defun roll-deterministic ()
  (let ((ret *deterministic*))
    (incf *deterministic*)
    (incf *deterministic-rolls*)
    (if (< 100 *deterministic*)
        (setf *deterministic* 1))
    ret))

(defun score-deterministic (losing)
  (* losing *deterministic-rolls*))

(defun move-deterministic (cur)
  (let ((new-pos cur))
    (dotimes (n 3)
      (setf new-pos (1+ (mod (+ (1- new-pos) (roll-deterministic)) 10))))
    new-pos))

(defun play-deterministic (init-1 init-2)
  (loop with score-1 = 0
        with score-2 = 0
        with pos-1 = init-1
        with pos-2 = init-2
        do (progn
             (setf pos-1 (move-deterministic pos-1)
                   score-1 (+ score-1 pos-1))
             (if (<= 1000 score-1)
                 (return-from play-deterministic (score-deterministic score-2)))
             (setf pos-2 (move-deterministic pos-2)
                   score-2 (+ score-2 pos-2))
             (if (<= 1000 score-2)
                 (return-from play-deterministic (score-deterministic score-1))))))

(defun part-1 ()
  (reset-deterministic)
  (play-deterministic 1 6))

(defparameter *quantum-rolls* '((3 1) (4 3) (5 6) (6 7) (7 6) (8 3) (9 1)))
(defvar *total-1* 0)
(defvar *total-2* 0)

(defun reset-quantum ()
  (setf *total-1* 0
        *total-2* 0))

(defun play-quantum (pos-1 score-1 pos-2 score-2 player worlds)
  (flet ((new-pos (adv pos) (1+ (mod (+ (1- pos) adv) 10)))) 
    (if (= 1 player)
        (loop for (advance num-worlds) in *quantum-rolls*
              do (let* ((new-pos-1 (new-pos advance pos-1))
                        (new-score-1 (+ score-1 new-pos-1)))
                   (if (<= 21 new-score-1)
                       (incf *total-1* (* num-worlds worlds))
                       (play-quantum new-pos-1 new-score-1 pos-2 score-2 2 (* worlds num-worlds)))))
        (loop for (advance num-worlds) in *quantum-rolls*
              do (let* ((new-pos-2 (new-pos advance pos-2))
                        (new-score-2 (+ score-2 new-pos-2)))
                   (if (<= 21 new-score-2)
                       (incf *total-2* (* num-worlds worlds))
                       (play-quantum pos-1 score-1 new-pos-2 new-score-2 1 (* worlds num-worlds))))))))
        
                     
(defun part-2 ()
  (reset-quantum)
  (play-quantum 1 0 6 0 1 1)
  (if (< *total-1* *total-2*)
      *total-2*
      *total-1*))
