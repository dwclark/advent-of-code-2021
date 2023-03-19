(defpackage :day-23a
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :alist-hash-table :define-constant :hash-table-keys :plist-hash-table
                :hash-table-values :copy-hash-table :maphash-keys :rcurry :hash-table-alist)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 0)))

(in-package :day-23a)

(defstruct (node (:conc-name nil))
  node-id coordinate room-for room-pair neighbors)

(defstruct (board (:conc-name nil))
  (positions 0 :type fixnum) (energy 0 :type fixnum) (heuristic 0 :type fixnum))

(defparameter *byte-specs*
  (plist-hash-table (list :a (vector (byte 8 0) (byte 8 8))
                          :b (vector (byte 8 16) (byte 8 24))
                          :c (vector (byte 8 32) (byte 8 40))
                          :d (vector (byte 8 48) (byte 8 56)))))

(defun index->letter (i)
  (ecase i ((0 1) :a)
         ((2 3) :b)
         ((4 5) :c)
         ((6 7) :d)))

(defparameter *energies*
  (plist-hash-table (list :a 1 :b 10 :c 100 :d 1000)))

(defparameter *nodes-list*
  (list (make-node :node-id 1 :coordinate '(0 . 1)
                   :room-for nil :neighbors '(2))
        (make-node :node-id 2 :coordinate '(0 . 2)
                   :room-for nil :neighbors '(1 3))
        (make-node :node-id 3 :coordinate '(0 . 3)
                   :room-for nil :neighbors '(2 4 13))
        (make-node :node-id 4 :coordinate '(0 . 4)
                   :room-for nil :neighbors '(3 5))
        (make-node :node-id 5 :coordinate '(0 . 5)
                   :room-for nil :neighbors '(4 6 15))
        (make-node :node-id 6 :coordinate '(0 . 6)
                   :room-for nil :neighbors '(5 7))
        (make-node :node-id 7 :coordinate '(0 . 7)
                   :room-for nil :neighbors '(6 8 17))
        (make-node :node-id 8 :coordinate '(0 . 8)
                   :room-for nil :neighbors '(7 9))
        (make-node :node-id 9 :coordinate '(0 . 9)
                   :room-for nil :neighbors '(8 10 19))
        (make-node :node-id 10 :coordinate '(0 . 10)
                   :room-for nil :neighbors '(9 11))
        (make-node :node-id 11 :coordinate '(0 . 11)
                   :room-for nil :neighbors '(10))
        (make-node :node-id 13 :coordinate '(1 . 3)
                   :room-for :a :room-pair 23 :neighbors '(3 23))
        (make-node :node-id 15 :coordinate '(1 . 5)
                   :room-for :b :room-pair 25 :neighbors '(5 25))
        (make-node :node-id 17 :coordinate '(1 . 7)
                   :room-for :c :room-pair 27 :neighbors '(7 27))
        (make-node :node-id 19 :coordinate '(1 . 9)
                   :room-for :d :room-pair 29 :neighbors '(9 29))
        (make-node :node-id 23 :coordinate '(2 . 3)
                   :room-for :a :neighbors '(13))
        (make-node :node-id 25 :coordinate '(2 . 5)
                   :room-for :b :neighbors '(15))
        (make-node :node-id 27 :coordinate '(2 . 7)
                   :room-for :c :neighbors '(17))
        (make-node :node-id 29 :coordinate '(2 . 9)
                   :room-for :d :neighbors '(19))))

(defparameter *nodes-destinations*
  (reduce #'(lambda (table node)
              (if (room-for node)
                  (let ((vec (gethash (room-for node) table (make-array 2 :fill-pointer 0))))
                    (vector-push node vec)
                    (setf (gethash (room-for node) table) vec)))
              table)
          *nodes-list*
          :initial-value (make-hash-table)))

(defparameter *finished*
  (loop with tmp = (make-hash-table)
        for room-sym being the hash-keys in *nodes-destinations* using (hash-value nodes-vec)
        do (setf (gethash room-sym tmp) (map 'vector #'node-id nodes-vec))
        finally (return (pack-positions tmp))))

(defun finished-p (packed-positions)
  (= packed-positions *finished*))

(defparameter *nodes-table*
  (reduce #'(lambda (table node)
              (setf (gethash (node-id node) table) node)
              table)
          *nodes-list*
          :initial-value (make-hash-table)))

(defun make-positions-table ()
  (plist-hash-table (list :a (make-array 2 :fill-pointer 0)
                          :b (make-array 2 :fill-pointer 0)
                          :c (make-array 2 :fill-pointer 0)
                          :d (make-array 2 :fill-pointer 0))))

(defun pack-positions (table)
  (loop with ret = 0
        for sym being the hash-keys in table using (hash-value values)
        do (let ((specs (gethash sym *byte-specs*)))
             (loop for num across values
                   for spec across specs
                   do (setf ret (logior (dpb num spec ret)))))
        finally (return ret)))

(defun unpack-positions (num)
  (loop with table = (make-positions-table)
        for sym being the hash-keys in *byte-specs* using (hash-value specs)
        do (loop for spec across specs
                 do (vector-push (ldb spec num) (gethash sym table)))
        finally (return table)))

(defun manhattan (n1 n2)
  (flet ((row (n) (car (coordinate n)))
         (col (n) (cdr (coordinate n))))
    (+ (abs (- (row n1) (row n2)))
       (abs (- (col n1) (col n2))))))

(defun hall-p (n)
  (<= 1 (node-id n) 11))

(defun room-p (n)
  (not (hall-p n)))

(defun stop-p (n)
  (let ((id (node-id n)))
    (or (= id 3) (= id 5) (= id 7) (= id 9))))

(defun occupied-p (node-id packed-positions)
  (loop for specs being the hash-values in *byte-specs*
        do (loop for spec across specs
                 do (if (= node-id (ldb spec packed-positions))
                        (return-from occupied-p t))))
  nil)
        
(defun estimated-remaining-cost (packed)
  (loop with total = 0
        for room-sym being the hash-keys in *nodes-destinations* using (hash-value nodes-vec)
        do (let* ((specs (gethash room-sym *byte-specs*))
                  (per-move (gethash room-sym *energies*))
                  (room-1 (aref nodes-vec 0))
                  (room-2 (aref nodes-vec 1))
                  (node-1 (gethash (ldb (aref specs 0) packed) *nodes-table*))
                  (node-2 (gethash (ldb (aref specs 1) packed) *nodes-table*)))

             (incf total (* per-move (min (+ (manhattan room-1 node-1)
                                             (manhattan room-2 node-2))
                                          (+ (manhattan room-1 node-2)
                                             (manhattan room-2 node-1))))))
        finally (return total)))
                  
  

#|(defun next-board (the-board idx new-id distance)
  (let* ((letter (index->letter idx))
         (specs (gethash letter *byte-specs*))
         (pack (positions the-board))
         (val-0 (if (evenp idx) new-id (ldb (aref specs 0) pack)))
         (val-1 (if (oddp idx) new-id (ldb (aref specs 1) pack)))
         (new-energy (+ (energy the-board) (* distance (gethash letter *energies*)))))
    (setf pack (dpb (min val-0 val-1) (aref specs 0) pack))
    (setf pack (dbp (max val-0 val-1) (aref specs 1) pack))
    (make-board :positions pack
                :energy new-energy
                :heuristic (+ new-energy (estimated-remaining-cost pack)))))
  |#
  
;(defun can-visit-p (letter start-node dest-node)
;  nil)
  ;; (if (and (eq :hall (cell-type start-node))
  ;;          (eq :hall (cell-type dest-node)))
  ;;     (return-from can-visit-p nil))

  ;; (if (and (eq :room (cell-type start-node))
  ;;          (eq :hall (cell-type dest-node))
  ;;          (stop-p dest-node))
  ;;     (return-from can-visit-p t))



#|(defun next-boards (the-board)
  (let ((current-positions (unpack-positions (positions the-board)))
        (ret (make-array 8 :adjustable t :fill-pointer 0))
        (frontier nil)
        (visited (make-hash-table :size 32)))
    (dotimes (idx (length current-positions))
      (setf frontier nil)
      (clrhash visited)
      (let* ((start-id (aref current-positions idx))
             (start-letter (index->letter idx))
             (start-node (gethash start-id *nodes*)))
        ;;need to prevent processing of any nodes that can't leave where they are now
        
        ;;prevent any processing of start node, we are already here
        (setf (gethash start-id visited) t)
        (dolist (neighbor-id (neighbors start-node))
          (push neighbor-id frontier))
                             
        (loop while frontier
              do (destructuring-bind (current-id . distance) (pop frontier)
                   (let ((current-node (gethash current-id *nodes*))) 
                     
                     (setf (gethash current-id visited) t)
                     
                     (if (not (find current-id current-positions))
                         (progn
                           (if (can-visit-p start-letter start-node current-node)
                               (vector-push-extend (next-board the-board idx current-id distance) ret))
                           
                           (dolist (neighbor-id (neighbors current-node))
                             (if (not (gethash neighbor-id visited))
                                 (push (cons neighbor-id (1+ distance)) frontier))))))))))
    ret))|#

(defun extract-positions (arg)
  (loop with lst = (cdr arg)
        with tmp-table = (make-positions-table)
        for str in lst
        for row from 0 to (length lst)
        do (loop for ch across str
                 for col from 0 to (length str)
                 do (let ((sym (intern (make-string 1 :initial-element ch) :keyword))
                          (id (parse-integer (format nil "~A~A" row col))))
                      (if (member sym '(:a :b :c :d))
                          (vector-push id (gethash sym tmp-table)))))
        finally (return (loop for key being the hash-keys in tmp-table using (hash-value val)
                              do (sort val #'<)
                              finally (return tmp-table)))))

