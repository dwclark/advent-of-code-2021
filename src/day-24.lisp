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

(defstruct alu
  (input 10 :type fixnum)
  (w 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum)
  start-at)

(defun copy-state (from-alu to-alu)
  (setf (alu-w to-alu) (alu-w from-alu))
  (setf (alu-x to-alu) (alu-x from-alu))
  (setf (alu-y to-alu) (alu-y from-alu))
  (setf (alu-z to-alu) (alu-z from-alu)))

(defun reset-state (alu &optional reset-input)
  (setf (alu-w alu) 0)
  (setf (alu-x alu) 0)
  (setf (alu-y alu) 0)
  (setf (alu-z alu) 0)
  (if reset-input
      (setf (alu-input alu) 10)))

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
        (b (if (= 3 (length instruction)) (third instruction) nil)))
        
    (ecase cmd
      (inp (write-alu alu a (alu-input alu)))
      
      (add (write-alu alu a
                      (+ (read-alu alu a)
                         (read-alu alu b))))
      
      (mul (write-alu alu a
                      (* (read-alu alu a)
                         (read-alu alu b))))
      
      (div (let ((val (read-alu alu b)))
             (if (zerop val)
                 nil
                 (write-alu alu a
                            (floor (/ (read-alu alu a) val))))))
      
      (mod (let ((val-a (read-alu alu a))
                 (val-b (read-alu alu b)))
             (if (or (< val-a 0) (<= val-b 0))
                 nil
                 (write-alu alu a (mod val-a val-b)))))
      
      (eql (if (eql (read-alu alu a) (read-alu alu b))
               (write-alu alu a 1)
               (write-alu alu a 0))))))

(defun instructions->alus (instructions)
  (loop with num-inp = (count 'inp instructions :test 'eq :key #'car)
        with ret = (make-array num-inp :initial-element nil)
        with idx = -1
        for instruction on instructions
        do (if (eq (first (car instruction)) 'inp)
               (setf (aref ret (incf idx)) (make-alu :start-at instruction)))
        finally (return ret)))
