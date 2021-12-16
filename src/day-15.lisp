(defpackage :day-15
  (:use :cl)
  (:import-from :utils :read-2d-world :each-neighbor/4)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :hash-table-alist :ensure-gethash)
  (:import-from :cl-heap :decrease-key :fibonacci-heap :pop-heap :add-to-heap)
  (:export #:part-1 #:part-2))

(in-package :day-15)

(defstruct node distance index coordinates)

(defun initial-node (v h)
  (if (and (= 0 v) (= 0 h))
      (make-node :distance 0 :index 0 :coordinates (cons 0 0))
      (make-node :distance most-positive-fixnum :index 0 :coordinates (cons v h))))

(defun node-key (node &optional new-val)
  (if new-val
      (setf (node-distance node) new-val)
      (node-distance node)))

(defun node-v (node)
  (car (node-coordinates node)))

(defun node-h (node)
  (cdr (node-coordinates node)))

;Dijkstra using min-priority-queue
(defun minimal-cost (world)
  (let* ((nodes (make-hash-table :test #'equal))
         (pq (make-instance 'fibonacci-heap :key #'node-key))
         (max-v (array-dimension world 0))
         (max-h (array-dimension world 1))
         (finish (cons (1- max-v) (1- max-h))))
    
    ;; initialize indexes and heap
    (loop for v from 0 below max-v
          do (loop for h from 0 below max-h
                   do (multiple-value-bind (node index) (add-to-heap pq (initial-node v h))
                        (setf (node-index node) index)
                        (setf (gethash (node-coordinates node) nodes) node))))

    (flet ((get-current ()
             (let ((ret (pop-heap pq)))
               (remhash (node-coordinates ret) nodes)
               ret)))
      
      (let ((current (get-current)))
        (flet ((process-neighbor (v h)
                 (let ((node (gethash (cons v h) nodes)))
                   (if node
                       (let ((new-distance (+ (aref world v h) (node-distance current))))
                         (if (< new-distance (node-distance node))
                             (decrease-key pq (node-index node) new-distance)))))))
          
          (loop while current
                do (if (equal (node-coordinates current) finish)
                       (return-from minimal-cost (node-distance current))
                       (progn
                         (each-neighbor/4 world (node-v current) (node-h current) #'process-neighbor)
                         (setf current (get-current))))))))
    nil))

(defun expand-world (world)
  (let* ((max-v (array-dimension world 0))
         (max-h (array-dimension world 1))
         (new-max-v (* 5 max-v))
         (new-max-h (* 5 max-h))
         (new-world (make-array (list new-max-v new-max-h))))
    (labels ((next-value (current augment)
               (let ((ret current))
                 (dotimes (n augment)
                   (if (= 9 ret)
                       (setf ret 1)
                       (incf ret)))
                 ret))
             
             (position-augment (start-v start-h augment)
               (loop for v from 0 below max-v
                     do (loop for h from 0 below max-h
                              do (let* ((current-val (aref world v h))
                                        (new-val (next-value current-val augment)))
                                   (setf (aref new-world (+ start-v v) (+ start-h h)) new-val))))))

      (loop for multiply-v from 0 below 5
            do (loop for multiply-h from 0 below 5
                     do (position-augment (* multiply-v max-v) (* multiply-h max-h) (+ multiply-v multiply-h)))))

    new-world))
        
(defun part-1 ()
  (let ((world (read-2d-world "15")))
    (minimal-cost world)))
  
(defun part-2 ()
  (let ((world (expand-world (read-2d-world "15"))))
    (minimal-cost world)))
