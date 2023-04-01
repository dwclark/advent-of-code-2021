;; I'm leaving this as a record of attempts (unsuccessful) to solve this problem
;; The real way to solve this one is by hand and recognizing what the code is
;; actually doing. I got a small part of the way there, but not going to lie,
;; I only solved this by adapting the code here:
;; https://www.ericburden.work/blog/2022/01/05/advent-of-code-2021-day-24/
;; to my own input and then working out the values by hand.

(defpackage :day-24
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-24)

(declaim (optimize (debug 3)))

(defun file->instructions ()
  (mapcar #'(lambda (line)
              (with-input-from-string (stream line)
                (loop for sym = (read stream nil :eof) ; stream, no error, :eof value
                      until (eq sym :eof)
                      collecting sym)))
          (read-day-file "24")))

(defun optimize-instructions (instructions)
  (let ((pass-1 (remove-if #'(lambda (i)
                               (or (and (eq 'div (first i))
                                        (eql 1 (third i)))
                                   (and (eq 'mul (first i))
                                        (eq 'x (second i))
                                        (eql 0 (third i))))) instructions)))
    pass-1))

(defstruct alu
  (input 0 :type fixnum)
  (w 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum)
  next
  start-at)

(defun alu->state (alu)
  (list (alu-x alu) (alu-y alu) (alu-z alu)))

(defun read-alu (alu sym)
  (if (numberp sym)
      sym
      (ecase sym (w (alu-w alu))
             (x (alu-x alu))
             (y (alu-y alu))
             (z (alu-z alu)))))

(defun write-alu (alu sym val)
  (ecase sym (w (setf (alu-w alu) val))
         (x (setf (alu-x alu) val))
         (y (setf (alu-y alu) val))
         (z (setf (alu-z alu) val))))

(defun execute-instruction (instruction alu)
  (let ((cmd (first instruction))
        (a (second instruction))
        (b (third instruction)))
        
    (ecase cmd
      (inp (write-alu alu a (alu-input alu)))
      
      (add (write-alu alu a
                      (+ (read-alu alu a)
                         (read-alu alu b))))
      
      (mul (write-alu alu a
                      (* (read-alu alu a)
                         (read-alu alu b))))
      
      (div (let ((val-a (read-alu alu a))
                 (val-b (read-alu alu b)))

             (if (zerop val-b)
                 nil
                 (write-alu alu a
                            (floor (/ val-a val-b))))))
      
      (mod (let ((val-a (read-alu alu a))
                 (val-b (read-alu alu b)))
             (if (or (< val-a 0) (<= val-b 0))
                 nil
                 (write-alu alu a (mod val-a val-b)))))
      
      (eql (if (eql (read-alu alu a) (read-alu alu b))
               (write-alu alu a 1)
               (write-alu alu a 0))))))

(defun instructions->alus (instructions)
  (first (maplist #'(lambda (sub)
                      (setf (alu-next (car sub)) (cadr sub))
                      (car sub))
                  
                  (remove-if #'null
                             (maplist #'(lambda (ins)
                                          (if (eq 'inp (caar ins))
                                              (make-alu :start-at ins)
                                              nil))
                                      instructions)))))

(defun execute-alu (self y z)
  (setf (alu-input self) 9
        (alu-x self) 0
        (alu-y self) y
        (alu-z self) z)
  
  (labels ((reset ()
             (decf (alu-input self))
             (setf (alu-x self) 0 (alu-y self) y (alu-z self) z))
           (executable (i)
             (not (or (null i) (eq 'inp (car i))))))

    (loop with valid = nil
          while (setf valid (and (<= 1 (alu-input self)) (execute-instruction (car (alu-start-at self)) self)))
          do (loop for instruction in (cdr (alu-start-at self))
                   while (and valid (executable instruction))
                   do (setf valid (execute-instruction instruction self))
                   finally (progn
                             (if valid
                                 (if (alu-next self)
                                     (setf valid (execute-alu (alu-next self) (alu-y self) (alu-z self)))
                                     (setf valid (zerop (alu-z self)))))

                             (if (not valid)
                                 (reset))))
             
          finally (return valid))))

(defun part-1 ()
  (let ((alu (instructions->alus (optimize-instructions (file->instructions)))))
    (execute-alu alu 0 0)
    (loop with my-alu = alu
          while my-alu
          collecting (format nil "~A" (alu-input my-alu)) into str-list
          do (setf my-alu (alu-next my-alu))
          finally (return (parse-integer (format nil "~{~A~}" str-list))))))
