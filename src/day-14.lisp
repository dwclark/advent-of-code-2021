(defpackage :day-14
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :hash-table-alist :ensure-gethash)
  (:export #:part-1 #:part-2))

(in-package :day-14)

;;;transforms are returned in a hashtable in this pattern: [ (first-letter . second-letter): transform ]
(defun load-data ()
  (let* ((initial (read-day-file "14"))
         (template (car initial))
         (hash (make-hash-table :test #'equal)))
    (loop for instruction in (cddr initial)
          do (do-register-groups (pattern inserts) ("^([A-Z]{2}) -> ([A-Z])$" instruction)
               (setf (gethash (cons (aref pattern 0) (aref pattern 1)) hash) (aref inserts 0))))
    (values template hash)))

;;;transform the template to reflect the keys in load-data
;;;returned table is in this pattern [ ((first-letter . second-letter) 'regular-or-last): count ]
(defun template->augment (template)
  (loop with hash = (make-hash-table :test #'equal)
        for i from 0 below (1- (length template))
        do (ensure-gethash (list (cons (aref template i) (aref template (1+ i)))
                                 (if (= (length template) (+ 2 i)) 'last 'regular)) hash 1)
        finally (return hash)))

;;; Basic algorithm
;;; 1) extract the first-letter/second-letter pattern from the augment table
;;; 2) look up the transformation in the table (same key as is the first atom in the list key in augment)
;;; 3) add data to new-augment for the first-letter + insert letter with 'regular type
;;; 4) add data to new-augment for the insert + second-letter with the correct type
(defun polymer-step (augment table)
  (loop with new-augment = (make-hash-table :test #'equal)
        for (cell type) being the hash-keys in augment using (hash-value c)
        do (let ((transform (gethash cell table))
                 (last-type (if (eq 'regular type) 'regular 'last)))
             (incf (gethash (list (cons (car cell) transform) 'regular) new-augment 0) c)
             (incf (gethash (list (cons transform (cdr cell)) last-type) new-augment 0) c))
        finally (return new-augment)))

;;; gather counts. If the cell is a 'regular type then only add in the counts for the first letter
;;; in the cons cell. If it is a 'last type, also add the counts for second letter. Once completed,
;;; transform the hash table to an a-list, sort the cells by the count, then subtract the last count (biggest)
;;; from the first count (smallest)
(defun diff-counts (augment)
  (loop with totals = (make-hash-table :test #'equal)
        for ((f . s) type) being the hash-keys in augment using (hash-value c)
        do (progn
             (incf (gethash f totals 0) c)
             (if (eq type 'last)
                 (incf (gethash s totals 0) c)))
        finally (let* ((alist (hash-table-alist totals))
                       (sorted (sort alist #'(lambda (c1 c2)
                                               (< (cdr c1) (cdr c2))))))
                  (return (- (cdar (last sorted))
                             (cdr (first sorted)))))))

(defun run-times (times)
  (multiple-value-bind (template table) (load-data)
    (let ((augment (template->augment template)))
      (dotimes (n times)
        (setf augment (polymer-step augment table)))
      (diff-counts augment))))

(defun part-1 ()
  (run-times 10))

(defun part-2 ()
  (run-times 40))
