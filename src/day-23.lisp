(defpackage :day-23
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :alist-hash-table :define-constant :hash-table-keys :plist-hash-table
   :hash-table-values :copy-hash-table :maphash-keys :rcurry :hash-table-alist)
  (:import-from :metabang.cl-containers :set-container :stack-container :insert-item :find-item :empty! :pop-item :insert-list :empty-p :pop-item)
  (:import-from :cl-heap :decrease-key :fibonacci-heap :pop-heap :add-to-heap)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-23)

(defparameter *letters* '(:a :b :c :d))
(defparameter *energies* (plist-hash-table (list :a 1 :b 10 :c 100 :d 1000)))
(defparameter *finished-positions*
  (plist-hash-table (list :a (vector '(2 . 3) '(3 . 3) '(4 . 3) '(5 . 3)) :b (vector '(2 . 5) '(3 . 5) '(4 . 5) '(5 . 5))
                          :c (vector '(2 . 7) '(3 . 7) '(4 . 7) '(5 . 7)) :d (vector '(2 . 9) '(3 . 9) '(4 . 9) '(5 . 9)))))

(defparameter *hash-bit-size* (integer-length most-positive-fixnum))
(defparameter *hash-bit-spec* (byte *hash-bit-size* 0))

(defstruct board a b c d (energy 0) queue-node)

(defun board-with-rooms (room-size)
  (make-board :a (make-array room-size :fill-pointer 0)
              :b (make-array room-size :fill-pointer 0)
              :c (make-array room-size :fill-pointer 0)
              :d (make-array room-size :fill-pointer 0)
              :energy 0))

(defun letter->positions (the-board letter)
  (ecase letter (:a (board-a the-board))
         (:b (board-b the-board))
         (:c (board-c the-board))
         (:d (board-d the-board))))

(defun next-board (prev letter new-idx new-cons distance)
  (labels ((sort-cons (c1 c2)
             (or (< (car c1) (car c2))
                 (< (cdr c1) (cdr c2))))

           (next-array (for-letter)
             (let ((prev-positions (letter->positions prev for-letter)))
               (if (not (eq letter for-letter))
                   prev-positions
                   (loop with new-array = (make-array (length prev-positions) :fill-pointer 0)
                         for element across prev-positions
                         for idx from 0 below (length prev-positions)
                         do (vector-push (if (= idx new-idx) new-cons (aref prev-positions idx)) new-array)
                         finally (return (sort new-array #'sort-cons)))))))
                                           
    (make-board :a (next-array :a)
                :b (next-array :b)
                :c (next-array :c)
                :d (next-array :d)
                :energy (+ (board-energy prev) (* distance (gethash letter *energies*))))))

(defun board-equal (b1 b2)
  (and (equalp (board-a b1) (board-a b2))
       (equalp (board-b b1) (board-b b2))
       (equalp (board-c b1) (board-c b2))
       (equalp (board-d b1) (board-d b2))))

(defun board-hash (b)
  (loop with ret = 0
        for letter in *letters*
        do (loop for (row . col) across (letter->positions b letter)
                 do (progn
                      (setf ret (+ (* 31 (+ (* 31 ret) row)) col))
                      (setf ret (ldb *hash-bit-spec* ret))))
        finally (return ret)))

(defun board-key (b &optional new-val)
  (if new-val
      (setf (board-energy b) new-val)
      (board-energy b)))

(defun board->occupied (b)
  (loop with ret = (make-instance 'set-container :test #'equal)
        for letter in *letters*
        do (loop for cell across (letter->positions b letter)
                 do (insert-item ret cell))
        finally (return ret)))

(defun hall-p (id)
  (= 1 (car id)))

(defun room-p (id)
  (< 1 (car id)))

(defun stop-p (id)
  (member id '((1 . 3) (1 . 5) (1. 7) (1 . 9)) :test #'equal))

(defun finished-p (the-board)
  (loop for letter in *letters*
        do (let ((should-be (gethash letter *finished-positions*))
                 (on-board (letter->positions the-board letter)))
             (loop for idx from 0 below (length on-board)
                   do (if (not (equal (aref should-be idx) (aref on-board idx)))
                          (return-from finished-p nil))))
        finally (return t)))

(defun final-place-p (the-board letter idx)
  (let* ((current-positions (letter->positions the-board letter))
         (finished-positions (gethash letter *finished-positions*)))

    (loop for i from idx below (length current-positions)
          do (if (not (equal (aref finished-positions i)
                             (aref current-positions i)))
                 (return-from final-place-p nil))
          finally (return t))))

(defun target-for-room-p (the-board letter to-id)
  (let* ((current-positions (letter->positions the-board letter))
         (finished-positions (gethash letter *finished-positions*))
         (target-idx (position to-id finished-positions :test #'equal)))
    (if target-idx
        (loop for i from target-idx below (length current-positions)
              do (if (not (equal (aref finished-positions i)
                                 (aref current-positions i)))
                     (return-from target-for-room-p nil))
              finally (return t))
        nil)))

(defun legal-move-p (the-board letter idx to-id)
  (let* ((current-positions (letter->positions the-board letter))
         (from-id (aref current-positions idx)))

    (if (and (hall-p from-id)
             (hall-p to-id))
        (return-from legal-move-p nil))

    (if (and (room-p from-id)
             (hall-p to-id))
        (if (and (not (stop-p to-id))
                 (not (final-place-p the-board letter idx)))
            (return-from legal-move-p t)
            (return-from legal-move-p nil)))

    (if (and (hall-p from-id)
             (room-p to-id))
        (if (target-for-room-p the-board letter to-id)
            (return-from legal-move-p t)
            (return-from legal-move-p nil)))

    (if (and (room-p from-id)
             (room-p to-id))
        (if (and (not (final-place-p the-board letter idx))
                 (target-for-room-p the-board letter to-id))
            (return-from legal-move-p t)
            (return-from legal-move-p nil)))

    (error "should not have arrived here")))


(defun parse-grid-board (list-strings)
  (loop with row-size = (length list-strings)
        with col-size = (length (first list-strings))
        with the-grid = (make-hash-table :test #'equal)
        with the-board = (board-with-rooms (- row-size 3))
        for row-idx from 0 below row-size
        do (labels ((parse-item (r c)
                      (if (and (<= 0 r) (< r row-size)
                               (<= 0 c) (< c col-size))
                          (let ((item (elt (elt list-strings r) c)))
                            (if (not (eql #\# item))
                                item
                                nil))
                          nil))

                    (add-neighbor (ary r c)
                      (if (parse-item r c)
                          (vector-push (cons r c) ary)))

                    (parse-neighbors (r c)
                      (let ((ret (make-array 3 :fill-pointer 0)))
                        (add-neighbor ret (1+ r) c)
                        (add-neighbor ret (1- r) c)
                        (add-neighbor ret r (1+ c))
                        (add-neighbor ret r (1- c))
                        ret)))
             
             (loop for col-idx from 0 below col-size
                   do (let* ((item (parse-item row-idx col-idx))
                             (sym (if item (intern (make-string 1 :initial-element item) :keyword) nil)))
                        (if item
                            (progn 
                              (setf (gethash (cons row-idx col-idx) the-grid) (parse-neighbors row-idx col-idx))
                              (if (member sym *letters*)
                                  (vector-push (cons row-idx col-idx) (letter->positions the-board sym))))))))
        finally (return (values the-grid the-board))))

(defvar *grid* nil)
(defvar *priority-queue* nil)
(defvar *visited* nil)

(defun add-next-board (next)
  (let ((previous (gethash next *visited*)))
    
    (cond ((not previous)
           (setf (board-queue-node next) (second (multiple-value-list (add-to-heap *priority-queue* next))))
           (setf (gethash next *visited*) next))

          ((and previous (< (board-energy next) (board-energy previous)))
           (decrease-key *priority-queue* (board-queue-node previous) (board-energy next))))))

(defun schedule-moves-from-position (current-board)
  (loop with occupied = (board->occupied current-board)
        with visited = (make-instance 'set-container :test #'equal)
        with stack = (make-instance 'stack-container)
        for letter in *letters*
        do (labels ((add-neighbors (id current-distance)
                      (loop for neighbor-id across (gethash id *grid*)
                            do (if (not (find-item visited neighbor-id))
                                   (insert-item stack (cons neighbor-id (1+ current-distance))))))
                    
                    (reset-tracking (id)
                      (empty! visited)
                      (insert-item visited id)
                      (add-neighbors id 0)))

             (loop with start-positions = (letter->positions current-board letter)
                   for idx from 0 below (length start-positions)
                   for start-id across start-positions
                   do (progn
                        (reset-tracking start-id)

                        (loop while (not (empty-p stack))
                              do (destructuring-bind (next-id . distance) (pop-item stack)
                                   (insert-item visited next-id)
                                   (if (and (not (find-item occupied next-id))
                                            (not (final-place-p current-board letter idx)))
                                       (progn
                                         (add-neighbors next-id distance)

                                         (if (legal-move-p current-board letter idx next-id)
                                             (add-next-board (next-board current-board letter idx next-id distance))))))))))))

(defun part-1 ()
  (multiple-value-bind (grid start) (parse-grid-board (read-day-file "23"))
    (let ((*grid* grid)
          (*visited* (make-hash-table :test #'board-equal :hash-function #'board-hash))
          (*priority-queue* (make-instance 'fibonacci-heap :key #'board-key)))
      (add-next-board start)

      (loop for next = (pop-heap *priority-queue*) then (pop-heap *priority-queue*)
            while (not (finished-p next))
            do (schedule-moves-from-position next)
            finally (return (board-energy next))))))
