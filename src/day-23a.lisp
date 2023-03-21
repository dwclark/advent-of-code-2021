(defpackage :day-23a
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :alist-hash-table :define-constant :hash-table-keys :plist-hash-table
   :hash-table-values :copy-hash-table :maphash-keys :rcurry :hash-table-alist)
  (:import-from :metabang.cl-containers :set-container :stack-container :insert-item :find-item :empty! :pop-item :insert-list :empty-p :pop-item)
  (:import-from :cl-heap :decrease-key :fibonacci-heap :pop-heap :add-to-heap)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 0)))

(in-package :day-23a)

(defstruct (node (:conc-name nil))
  node-id coordinate room-for room-pair neighbors)

(defstruct (board (:conc-name nil))
  (positions 0 :type fixnum) (energy 0 :type fixnum) (heuristic 0 :type fixnum) queue-index)

(defun board-key (the-board &optional val)
  (if val
      (setf (energy the-board) val)
      (energy the-board)))
  
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
                 for idx from 0 below 2
                 do (setf (aref (gethash sym table) idx) (ldb spec num)))
        finally (return table)))

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
  (plist-hash-table (list :a (make-array 2) :b (make-array 2) :c (make-array 2) :d (make-array 2))))

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

(defun packed->occupied-set (packed-positions)
  (loop with ret = (make-instance 'set-container)
        for specs being the hash-values in *byte-specs*
        do (loop for spec across specs
                 do (insert-item ret (ldb spec packed-positions)))
        finally (return ret)))
  
(defun estimated-remaining-cost (unpacked)
  (loop with total = 0
        for room-sym being the hash-keys in *nodes-destinations* using (hash-value nodes-vec)
        do (let* ((per-move (gethash room-sym *energies*))
                  (current-ids (gethash room-sym unpacked))
                  (room-1 (aref nodes-vec 0))
                  (room-2 (aref nodes-vec 1))
                  (node-1 (gethash (aref current-ids 0) *nodes-table*))
                  (node-2 (gethash (aref current-ids 1) *nodes-table*)))

             (incf total (* per-move (min (+ (manhattan room-1 node-1)
                                             (manhattan room-2 node-2))
                                          (+ (manhattan room-1 node-2)
                                             (manhattan room-2 node-1))))))
        finally (return total)))

(defun final-place-p (letter idx pos-ary)
  (let ((node-destination (gethash letter *nodes-destinations*))
        (id (aref pos-ary idx)))
    (or (= id (node-id (aref node-destination 1))) ;in bottom of room
        (and (= id (node-id (aref node-destination 0))) ;in top of room
             (= (aref pos-ary 1) (node-id (aref node-destination 1)))))))

(defun target-room-p (letter to-node pos-ary)
  (and (eq letter (room-for to-node))
       (or (not (room-pair to-node))
           (= (room-pair to-node) (aref pos-ary 1)))))
  
(defun legal-move-p (letter idx unpacked to-id)
  (let* ((from-id (aref (gethash letter unpacked) idx))
         (pos-ary (gethash letter unpacked))
         (from-node (gethash from-id *nodes-table*))
         (to-node (gethash to-id *nodes-table*)))

    (if (and (hall-p from-node)
             (hall-p to-node))
        (return-from legal-move-p nil))

    (if (and (room-p from-node)
             (hall-p to-node))
        (if (and (not (stop-p to-node))
                 (not (final-place-p letter idx pos-ary)))
            (return-from legal-move-p t)
            (return-from legal-move-p nil)))

    (if (and (hall-p from-node)
             (room-p to-node))
        (if (target-room-p letter to-node pos-ary)
            (return-from legal-move-p t)
            (return-from legal-move-p nil)))

    (if (and (room-p from-node)
             (room-p to-node))
        (if (and (not (final-place-p letter idx pos-ary))
                 (target-room-p letter to-node pos-ary))
            (return-from legal-move-p t)
            (return-from legal-move-p nil)))

    (error "should not have arrived here")))

(defun extract-positions (arg)
  (loop with lst = (cdr arg)
        with tmp-table = (make-positions-table)
        with indexes = (plist-hash-table (list :a -1 :b -1 :c -1 :d -1))
        for str in lst
        for row from 0 to (length lst)
        do (loop for ch across str
                 for col from 0 to (length str)
                 do (let ((sym (intern (make-string 1 :initial-element ch) :keyword))
                          (id (parse-integer (format nil "~A~A" row col))))
                      (if (member sym '(:a :b :c :d))
                          (setf (aref (gethash sym tmp-table) (incf (gethash sym indexes))) id))))
        finally (return (loop for key being the hash-keys in tmp-table using (hash-value val)
                              do (sort val #'<)
                              finally (return tmp-table)))))
  
(defvar *tmp-positions-table* (make-positions-table))

(defun copy->tmp-table (unpacked)
  (loop for key being the hash-keys in unpacked using (hash-value vec)
        for target = (gethash key *tmp-positions-table*) then (gethash key *tmp-positions-table*)
        do (setf (aref target 0) (aref vec 0)
                 (aref target 1) (aref vec 1))))
  
(defun next-board (prev unpacked letter idx new-id distance)
  (copy->tmp-table unpacked)
  ;(format t "~A~%" (hash-table-alist unpacked))
  ;(format t "~A~%" (hash-table-alist *tmp-positions-table*))
  (let ((target-vec (gethash letter *tmp-positions-table*)))
    (setf (aref target-vec idx) new-id)
    (sort target-vec #'<)
    
    (make-board :positions (pack-positions *tmp-positions-table*)
                :energy (+ (energy prev) (* distance (gethash letter *energies*)))
                :heuristic (estimated-remaining-cost *tmp-positions-table*))))

(defvar *priority-queue* nil)
(defvar *visited* nil)

(defun add-next-board (the-board)
  (let* ((pos (positions the-board))
         (already (gethash pos *visited*)))

    (cond ((not already)
           (setf (queue-index the-board) (second (multiple-value-list (add-to-heap *priority-queue* the-board))))
           (setf (gethash pos *visited*) the-board))

          ((and already (< (energy the-board) (energy already)))
           (decrease-key *priority-queue* (queue-index already) (energy the-board))))))
          
(defun schedule-moves-from-position (current-board)
  ;(format t "in scheduled-moves-from-position: packed: ~A, energy: ~A, heuristic: ~A~%"
  ;        (positions current-board) (energy current-board) (heuristic current-board))
  
  (loop with packed = (positions current-board)
        with unpacked = (unpack-positions packed)
        with occupied = (packed->occupied-set packed)
        with visited = (make-instance 'set-container)
        with stack = (make-instance 'stack-container)
        for letter being the hash-keys in unpacked using (hash-value id-vec)
        do (labels ((add-neighbors (node current-distance)
                      (dolist (neighbor-id (neighbors node))
                        (if (not (find-item visited neighbor-id))
                            (insert-item stack (cons neighbor-id (1+ current-distance))))))

                    (reset-tracking (id)
                      (empty! visited)
                      (insert-item visited id)
                      (add-neighbors (gethash id *nodes-table*) 0)))
             
             (loop for idx from 0 below 2
                   for start-id across id-vec
                   do (progn
                        ;(format t "start-id: ~A~%" start-id)
                        (reset-tracking start-id)

                        (loop while (not (empty-p stack))
                              do (destructuring-bind (next-id . distance) (pop-item stack)
                                   (insert-item visited next-id)
                                   (if (and (not (find-item occupied next-id))
                                            (not (final-place-p letter idx id-vec)))
                                       (let ((next-node (gethash next-id *nodes-table*)))
                                         ;(format t "next-id: ~A next-node: ~A~%" next-id next-node)
                                         (add-neighbors next-node distance)

                                         (if (legal-move-p letter idx unpacked next-id)
                                             (add-next-board (next-board current-board unpacked letter idx next-id distance))))))))))))

(defun part-1 ()
  (let* ((input-list (read-day-file "23"))
         (unpacked (extract-positions input-list))
         (start-board (make-board :positions (pack-positions unpacked) :energy 0
                                  :heuristic (estimated-remaining-cost unpacked)))
         (*visited* (make-hash-table))
         (*priority-queue* (make-instance 'fibonacci-heap :key #'board-key)))
    (add-next-board start-board)

    (loop for next-board = (pop-heap *priority-queue*) then (pop-heap *priority-queue*)
          while (not (finished-p (positions next-board)))
          do (schedule-moves-from-position next-board)
          finally (return (energy next-board)))))
         
