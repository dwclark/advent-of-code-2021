(defpackage :day-22
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-22)

(defstruct (cuboid (:conc-name nil))
  (min-x 0)
  (max-x 0)
  (min-y 0)
  (max-y 0)
  (min-z 0)
  (max-z 0)
  (action :off))

(defun action- (a)
  (if (eq :on a) :off :on))

(defun cuboid-parse (line)
  (flet ((parse-action (val) (if (string= "on" val) :on :off)))
    (do-register-groups
        ((#'parse-action a) (#'parse-integer min-x) (#'parse-integer max-x)
         (#'parse-integer min-y) (#'parse-integer max-y)
         (#'parse-integer min-z) (#'parse-integer max-z))
        ("(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)" line)
      (return (make-cuboid :min-x min-x :max-x max-x :min-y min-y :max-y max-y :min-z min-z :max-z max-z :action a)))))

(defun cuboid-intersection (first second)
  (let ((possible (make-cuboid :min-x (max (min-x first) (min-x second)) :max-x (min (max-x first) (max-x second))
                               :min-y (max (min-y first) (min-y second)) :max-y (min (max-y first) (max-y second))
                               :min-z (max (min-z first) (min-z second)) :max-z (min (max-z first) (max-z second)))))
    (if (and (<= (min-x possible) (max-x possible))
             (<= (min-y possible) (max-y possible))
             (<= (min-z possible) (max-z possible)))
        possible
        nil)))

(defun cuboid-volume (c)
  (let ((vol (* (1+ (- (max-x c) (min-x c)))
                (1+ (- (max-y c) (min-y c)))
                (1+ (- (max-z c) (min-z c))))))
    (assert (< 0 vol))
    (if (eq :on (action c)) vol (- vol))))
        
(defun part-1-within (c)
  (and (<= -50 (min-x c) 50) (<= -50 (max-x c) 50)
       (<= -50 (min-y c) 50) (<= -50 (max-y c) 50)
       (<= -50 (min-z c) 50) (<= -50 (max-z c) 50)))

(defun count-on (cuboids)
  (loop with added = (make-array 0 :fill-pointer 0 :adjustable t)
        for current in cuboids
        do (let ((this-round (make-array 0 :fill-pointer 0 :adjustable t)))
             (if (eq :on (action current))
                 (vector-push-extend current this-round))
             (loop for tmp across added
                   do (let ((intersection (cuboid-intersection current tmp)))
                        (when intersection
                          (setf (action intersection) (action- (action tmp)))
                          (vector-push-extend intersection this-round)))
                   finally (loop for c across this-round do (vector-push-extend c added))))
        finally (progn
                  (return (reduce #'+ (map 'list #'cuboid-volume added))))))

(defun part-1 ()
  (let* ((lines (read-day-file "22"))
         (cuboids (remove-if-not #'part-1-within (mapcar #'cuboid-parse lines))))
    (count-on cuboids)))
         
(defun part-2 ()
  (let* ((lines (read-day-file "22"))
         (cuboids (mapcar #'cuboid-parse lines)))
    (count-on cuboids)))
