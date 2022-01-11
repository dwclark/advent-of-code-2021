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

(defun cuboid= (f s)
  (and (= (min-x f) (min-x s))
       (= (max-x f) (max-x s))
       (= (min-y f) (min-y s))
       (= (max-y f) (max-y s))
       (= (min-z f) (min-z s))
       (= (max-z f) (max-z s))))

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

(defun within-cuboid-p (x y z c)
  (and (<= (min-x c) x (max-x c))
       (<= (min-y c) y (max-y c))
       (<= (min-z c) z (max-z c))))

(defun find-action (x y z cuboids)
  (loop with action = :off
        for cuboid in cuboids
        do (if (within-cuboid-p x y z cuboid)
               (setf action (action cuboid)))
        finally (return action)))

(defun display-cuboids (cuboids)
  (loop for c in cuboids
        do (format t "~A ~A~%" c (cuboid-volume c))
        finally (format t "~%")))

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
                  ;;(display-cuboids (concatenate 'list added))
                  (return (reduce #'+ (map 'list #'cuboid-volume added))))))

(defun test-part-1 ()
  (let* ((lines (read-day-file "22"))
         (cuboids (remove-if-not #'part-1-within (mapcar #'cuboid-parse lines))))
    (count-on cuboids)))
         
(defun part-1 ()
  (let* ((lines (read-day-file "22"))
         (cuboids (remove-if-not #'part-1-within (mapcar #'cuboid-parse lines)))
         (total 0))
    (loop for x from -50 to 50
          do (loop for y from -50 to 50
                   do (loop for z from -50 to 50
                            do (if (eq :on (find-action x y z cuboids))
                                   (incf total)))))
    total))

(defun part-2 ()
  (let* ((lines (read-day-file "22"))
         (cuboids (mapcar #'cuboid-parse lines)))
    (count-on cuboids)))

(defun test-intersection-1 ()
  (let ((small (make-cuboid :min-x 1 :max-x 3 :min-y 1 :max-y 3 :min-z 1 :max-z 3))
        (large (make-cuboid :min-x 0 :max-x 4 :min-y 0 :max-y 4 :min-z 0 :max-z 4))
        (intersect (make-cuboid :min-x 1 :max-x 3 :min-y 1 :max-y 3 :min-z 1 :max-z 3)))
    (assert (cuboid= intersect (cuboid-intersection small large)))
    (assert (cuboid= intersect (cuboid-intersection large small)))))

(defun test-intersection-2 ()
  (let ((left (make-cuboid :min-x 0 :max-x 10 :min-y 0 :max-y 10 :min-z 0 :max-z 10))
        (right (make-cuboid :min-x 5 :max-x 15 :min-y 5 :max-y 15 :min-z 5 :max-z 15))
        (outside (make-cuboid :min-x 30 :max-x 40 :min-y 30 :max-y 40 :min-z 30 :max-z 40))
        (z-outside (make-cuboid :min-x 5 :max-x 10 :min-y 5 :max-y 10 :min-z 30 :max-z 40))
        (should-be (make-cuboid :min-x 5 :max-x 10 :min-y 5 :max-y 10 :min-z 5 :max-z 10)))
    
    (assert (cuboid= should-be (cuboid-intersection left right)))
    (assert (cuboid= should-be (cuboid-intersection right left)))
    (assert (not (cuboid-intersection left outside)))
    (assert (not (cuboid-intersection outside left)))
    (assert (not (cuboid-intersection right outside)))
    (assert (not (cuboid-intersection outside right)))
    (assert (not (cuboid-intersection z-outside left)))
    (assert (not (cuboid-intersection z-outside right)))))
    
(defun test-count-on ()
  (let ((first (make-cuboid :min-x 0 :max-x 10 :min-y 0 :max-y 10 :min-z 0 :max-z 1 :action :on))
        (second (make-cuboid :min-x 5 :max-x 15 :min-y 5 :max-y 15 :min-z 0 :max-z 1 :action :on))
        (third (make-cuboid :min-x 0 :max-x 2 :min-y 0 :max-y 2 :min-z 0 :max-z 1 :action :off))
        (fourth (make-cuboid :min-x 1 :max-x 2 :min-y 1 :max-y 2 :min-z 0 :max-z 1 :action :off))
        (fifth (make-cuboid :min-x 0 :max-x 1 :min-y 0 :max-y 1 :min-z 0 :max-z 1 :action :on))
        (sixth (make-cuboid :min-x 0 :max-x 1 :min-y 0 :max-y 1 :min-z 0 :max-z 1 :action :off)))
    (assert (= 100 (count-on (list first))))
    (assert (= 175 (count-on (list first second))))
    (assert (= 171 (count-on (list first second third))))
    (assert (= 171 (count-on (list first second third fourth))))
    (assert (= 172 (count-on (list first second third fourth fifth))))
    (assert (= 172 (count-on (list first second third fourth fifth fourth))))
    (assert (= 171 (count-on (list first second third fourth fifth fourth sixth))))
    
    ))

(defun test-single-interval ()
  (let ((first (make-cuboid :min-x 0 :max-x 2 :min-y 0 :max-y 1 :min-z 0 :max-z 1 :action :on))
        (second (make-cuboid :min-x 1 :max-x 3 :min-y 0 :max-y 1 :min-z 0 :max-z 1 :action :on))
        (third (make-cuboid :min-x 1 :max-x 2 :min-y 0 :max-y 1 :min-z 0 :max-z 1 :action :off))
        (fourth (make-cuboid :min-x 1 :max-x 2 :min-y 0 :max-y 1 :min-z 0 :max-z 1 :action :off))
        (fifth (make-cuboid :min-x 1 :max-x 2 :min-y 0 :max-y 1 :min-z 0 :max-z 1 :action :on)))
    (count-on (list first second third fourth fifth))))
