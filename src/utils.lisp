(defpackage :utils
  (:use :cl)
  (:export #:read-file #:read-day-file #:split-blank-lines #:read-blank-line-blocks
           #:*input-directory* 
           #:bit-vector->integer #:integer->bit-vector #:power-set))

(in-package :utils)

(defparameter *input-directory* nil)

(defun day-file-name (day)
  (concatenate 'string *input-directory* "day-" day ".txt"))

(defun read-file (file-name)
  (with-open-file (stm file-name)
    (loop for line = (read-line stm nil)
          while line
          collect line)))

(defun read-2d-world (day)
  (let ((init (mapcar #'(lambda (s) (map 'list #'digit-char-p s)) (read-day-file day))))
    (make-array (list (length init) (length (car init)))
                :initial-contents init)))

(defparameter *to-add/8* '((-1 . -1) (-1 . 0)  (-1 . 1)
                           (0 . -1)            (0 . 1)
                           (1 . -1)  (1 . 0)   (1 . 1)))

(defparameter *to-add/4* '(          (-1 . 0)
                           (0 . -1)            (0 . 1)
                                     (1 . 0)))

(defparameter *to-add/2* '((0 . 1) (1 . 0)))

(defun each-neighbor/8 (world row col func)
  (let ((max-row (array-dimension world 0))
        (max-col (array-dimension world 1)))
    (flet ((legal-p (r c)
             (and (<= 0 r) (<= 0 c)
                  (< r max-row) (< c max-col))))

      (loop for (add-row . add-col) in *to-add/8*
            do (let ((new-row (+ add-row row))
                     (new-col (+ add-col col)))
                 (if (legal-p new-row new-col)
                     (funcall func new-row new-col)))))))

(defun each-neighbor/4 (world row col func)
  (let ((max-row (array-dimension world 0))
        (max-col (array-dimension world 1)))
    (flet ((legal-p (r c)
             (and (<= 0 r) (<= 0 c)
                  (< r max-row) (< c max-col))))

      (loop for (add-row . add-col) in *to-add/4*
            do (let ((new-row (+ add-row row))
                     (new-col (+ add-col col)))
                 (if (legal-p new-row new-col)
                     (funcall func new-row new-col)))))))

(defun each-neighbor/2 (world row col func)
  (let ((max-row (array-dimension world 0))
        (max-col (array-dimension world 1)))
    (flet ((legal-p (r c)
             (and (<= 0 r) (<= 0 c)
                  (< r max-row) (< c max-col))))

      (loop for (add-row . add-col) in *to-add/2*
            do (let ((new-row (+ add-row row))
                     (new-col (+ add-col col)))
                 (if (legal-p new-row new-col)
                     (funcall func new-row new-col)))))))

(defun read-day-file (day)
  (read-file (concatenate 'string *input-directory* "day-" day ".txt")))

(defun read-blank-line-blocks (day)
  (let ((blocks nil)
        (current ""))
    (with-open-file (stm (day-file-name day))
      (loop for line = (read-line stm nil)
            while line
            do (if (< 0 (length line))
                   (progn (if (= 0 (length current))
                              (setf current line)
                              (setf current (concatenate 'string current (vector #\Newline) line))))
                   (progn
                     (push current blocks)
                     (setf current "")))
            finally (return (progn
                              (push current blocks)
                              (nreverse blocks)))))))

(defun load-numbers (day)
  (map 'vector #'parse-integer (read-day-file day)))

(defun power-set (arg-list)
  (if (null arg-list)
      (list nil)
      (let ((prev (power-set (rest arg-list))))
        (append (mapcar (lambda (e)
                          (append (list (first arg-list)) e))
                        prev)
                prev))))
                        
(defun split-blank-lines (lines)
  (let ((list-of-lists nil)
        (current nil))
    (loop for line in lines
          do (if (= 0 (length line))
                 (progn
                   (push (nreverse current) list-of-lists)
                   (setf current nil))
                 (push line current))
          finally (return (progn
                            (push (nreverse current) list-of-lists)
                            (nreverse list-of-lists))))))

(defun bit-vector->integer (bit-vector)
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (width arg)
  (loop for i from (1- width) downto 0
        with bitvec = (make-array width :element-type 'bit)
        with num = arg
        do (progn
             (setf (aref bitvec i) (rem num 2))
             (setf num (floor (/ num 2))))
        finally (return bitvec)))
