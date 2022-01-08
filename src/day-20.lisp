(defpackage :day-20
  (:use :cl)
  (:import-from :utils :read-blank-line-blocks)
  (:import-from :cl-ppcre :split :do-register-groups)
  (:import-from :alexandria :hash-table-alist :curry)
  (:import-from :metabang.cl-containers :basic-queue :insert-item :delete-first :empty-p)
  (:export #:part-1 #:part-2))

(in-package :day-20)

(defun get-v (cell)
  (car cell))

(defun get-h (cell)
  (cdr cell))

(defstruct (grid (:conc-name nil))
  (table (make-hash-table :test #'equal))
  (min-v 0)
  (max-v 0)
  (min-h 0)
  (max-h 0))

(defun add-grid (g v h val)
  (if (zerop val)
      (remhash (cons v h) (table g))
      (setf (gethash (cons v h) (table g)) 1))
  
  (when (< v (min-v g)) (setf (min-v g) v))
  (when (< (max-v g) v) (setf (max-v g) v))
  (when (< h (min-h g)) (setf (min-h g) h))
  (when (< (max-h g) h) (setf (max-h g) h)))

(defvar *step* 0)

(defun get-grid-value (g v h)
  (if (in-grid g v h)
      (gethash (cons v h) (table g) 0)
      (if (evenp *step*) 1 0)))

(defun get-grid-number (g v h)
  (loop with ret = 0
        with num-idx = 9
        for v-inc from -1 to 1
        do (loop for h-inc from -1 to 1
                 do (let ((v-idx (+ v v-inc))
                          (h-idx (+ h h-inc)))
                      (setf ret (logior ret (ash (get-grid-value g v-idx h-idx) (decf num-idx))))))
        finally (return ret)))

(defun in-grid (g v h)
  (and (<= (min-v g) v)
       (<= v (max-v g))
       (<= (min-h g) h)
       (<= h (max-h g))))
    
(defun translate->number (c)
  (if (eql c #\#) 1 0))

(defun parse-enhancer (line)
  (map 'vector #'translate->number line))

(defun enhanced (e num)
  (aref e num))

(defun parse-grid (str)
  (loop with lines = (split "\\n+" str)
        with grid = (make-grid)
        for v from 0 below (length lines)
        do (loop with line = (nth v lines)
                 for h from 0 below (length line)
                 do (add-grid grid v h (translate->number (aref line h))))
        finally (return grid)))

(defun parse-file ()
  (let ((lst (read-blank-line-blocks "20")))
    (values (parse-enhancer (first lst))
            (parse-grid (second lst)))))

(defun run-enhancement (enhancer grid)
  (loop with ret-grid = (make-grid)
        for v from (1- (min-v grid)) to (1+ (max-v grid))
        do (loop for h from (1- (min-h grid)) to (1+ (max-h grid))
                 do (let* ((num (get-grid-number grid v h))
                           (e (enhanced enhancer num)))
                      (add-grid ret-grid v h e)))
        finally (return ret-grid)))

(defun run-n-enhancements (n)
  (multiple-value-bind (enhancer grid) (parse-file)
    (let ((working grid))
      (dotimes (s n)
        (let ((*step* (1+ s)))
          (setf working (run-enhancement enhancer working))))
      (hash-table-count (table working)))))

(defun part-1 ()
  (run-n-enhancements 2))

(defun part-2 ()
  (run-n-enhancements 50))
  
          
