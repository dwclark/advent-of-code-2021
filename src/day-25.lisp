(defpackage :day-25
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-25)

(defvar *board* nil)
(defvar *row-max* 0)
(defvar *col-max* 0)

(defun read-board ()
  (loop with the-list = (read-day-file "25")
        with num-rows = (length the-list)
        with num-cols = (length (first the-list))
        with ret = (make-array (list num-rows num-cols) :initial-element :empty)
        for row in the-list
        for row-index from 0 below (length the-list)
        do (loop for col-index from 0 below (length row)
                 do (let ((c (elt row col-index)))
                      (setf (aref ret row-index col-index)
                            (cond ((char= c #\>) :east)
                                  ((char= c #\v) :south)
                                  (t :empty)))))
          finally (return ret)))

(defun next-location (row col)
  (let ((content (aref *board* row col)))
    (ecase content
      (:east (cons row (if (= (1+ col) *col-max*) 0 (1+ col))))
      (:south (cons (if (= (1+ row) *row-max*) 0 (1+ row)) col))
      (:empty nil))))

(defun schedule-moves (facing)
  (loop with moves = nil
        for from-row from 0 below *row-max*
        do (loop for from-col from 0 below *col-max*
                 do (if (eq facing (aref *board* from-row from-col))
                        (let ((to-move (next-location from-row from-col)))
                          (if (eq :empty (aref *board* (car to-move) (cdr to-move)))
                              (push (list (cons from-row from-col) to-move) moves)))))
        finally (return moves)))

(defun do-moves (moves)
  (loop for ((from-row . from-col) (to-row . to-col)) in moves
        do (rotatef (aref *board* from-row from-col)
                    (aref *board* to-row to-col))
        finally (return moves)))

(defun part-1 ()
  (let* ((*board* (read-board))
         (*row-max* (array-dimension *board* 0))
         (*col-max* (array-dimension *board* 1)))

    (loop with num-moves = 0
          with continue = t
          while continue
          do (let ((east-moves (do-moves (schedule-moves :east)))
                   (south-moves (do-moves (schedule-moves :south))))
               (setf continue (or east-moves south-moves))
               (if continue (incf num-moves)))
         finally (return (1+ num-moves)))))
