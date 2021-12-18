(defpackage :day-17
  (:use :cl)
  (:export #:part-1 #:part-2))

(in-package :day-17)

;; sorry, no parsing today, it's a single line, just search/replace when values change
(defun x-pos (v0 n)
  (let ((accum 0))
    (dotimes (s n)
      (setf accum (+ accum (if (<= v0 s) 0 (- v0 s)))))
    accum))

(defun y-pos (v0 n)
  (let ((pos 0))
    (dotimes (s n)
      (setf pos (+ pos (- v0 s))))
    pos))

(defun y-max (n)
  (loop for i from 1 to n summing i))

;;just iterate over the values starting from n = 1. Once the x-val > x-max
;;or y-val < y-min we can stop. Stop early if target hit is found
(defun hits-target (x-velocity y-velocity x-min x-max y-min y-max)
  (loop with x-pos = 0
        with y-pos = 0
        for n from 1 to most-positive-fixnum
        while (and (<= x-pos x-max) (<= y-min y-pos))
        do (progn
             (setf x-pos (x-pos x-velocity n) y-pos (y-pos y-velocity n))
             (if (and (<= x-min x-pos x-max) (<= y-min y-pos y-max))
                 (return-from hits-target t))))
  nil)

;;find the allowable x speeds, x has a minimal speed because at some point
;;it will stop travelling on the x axis. It also has a max speed, it can't be faster
;;than x-max otherwise it will overshoot x-max on the first try
(defun allowable-x-velocities (x-min x-max)
  (loop with inside = nil
        for v from 1 to x-max
        do (loop for n from 1 to (1+ v)
                 do (let ((pos (x-pos v n)))
                      (if (and (<= x-min pos) (<= pos x-max))
                          (push v inside))))
           
        finally (return (remove-duplicates inside))))

;;y-min velocity will be y-min, which is a straight shot down. We found
;;y-max velocity in part one. Now, just iterate over those ranges, using the
;;hits-target function to test if it will be in the target for some n.
(defun allowable-values (x-min x-max y-min y-max)
  (loop with y-min-velocity = y-min
        with y-max-velocity = (1- (abs y-min))
        with accum = nil
        for x-velocity in (allowable-x-velocities x-min x-max)
        do (loop for y-velocity from y-min-velocity to y-max-velocity
                 do (if (hits-target x-velocity y-velocity x-min x-max y-min y-max)
                        (push (cons x-velocity y-velocity) accum)))
        finally (return (remove-duplicates accum :test #'equal))))

;;target area: x=244..303, y=-91..-54
;;The trick is to realize that the velocity in the y-axis on the way down
;;will be precisely the same at 0 as the initial velocity. So if the probe
;;is shot up with n velocity, it will have n velocity at y = 0. The n + 1
;;step will need to hit the target area. This means that n+1 = abs(min-y),
;;or n = abs(min-y) - y. Since min in my case was -91, n = 90. Then just
;;use the y-max function to compute the highest point.
(defun part-1 ()
  (y-max 90))

(defun part-2 ()
  (length (allowable-values 244 303 -91 -54)))
