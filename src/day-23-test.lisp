(in-package :day-23)

(declaim (optimize (debug 3)))

(defun test-basic-operations ()
  (let ((b1 (make-board :a (vector '(1 . 1) '(1 . 2))
                        :b (vector '(1 . 4) '(1 . 6))
                        :c (vector '(1 . 7) '(1 . 8))
                        :d (vector '(1 .  10) '(1 . 11))))

        (next (make-board :a (vector '(1 . 1) '(1 . 2))
                        :b (vector '(1 . 4) '(2 . 6))
                        :c (vector '(1 . 7) '(1 . 8))
                        :d (vector '(1 .  10) '(1 . 11))))
        
        (b2 (make-board :a (vector '(1 . 1) '(1 . 2))
                        :b (vector '(2 . 4) '(2 . 6))
                        :c (vector '(2 . 7) '(2 . 8))
                        :d (vector '(1 .  10) '(1 . 11)))))

    (assert (board-equal b1 b1))
    (assert (not (board-equal b1 b2)))
    (assert (equalp (letter->positions b1 :c) (vector '(1 . 7) '(1 . 8))))
    (assert (eql (board-hash b1) (board-hash b1)))
    (assert (not (eql (board-hash b1) (board-hash b2))))
    (assert (board-equal (next-board b1 :b 1 '(2 . 6) 4) next))))

(defun test-cell-types ()
  (assert (hall-p '(1 . 10)))
  (assert (not (hall-p '(2 . 3))))
  (assert (stop-p '(1 . 3)))
  (assert (stop-p '(1 . 5)))
  (assert (not (stop-p '(2 . 3))))
  (assert (room-p '(2 . 3)))
  (assert (room-p '(3 . 9)))
  (assert (not (room-p '(1 . 3)))))

(defun test-finished ()
  (let ((finished-4 (make-board :a (gethash :a *finished-positions*)
                                :b (gethash :b *finished-positions*)
                                :c (gethash :c *finished-positions*)
                                :d (gethash :d *finished-positions*)))
        (finished-2 (make-board :a (vector '(2 . 3) '(3 . 3))
                                :b (vector '(2 . 5) '(3 . 5))
                                :c (vector '(2 . 7) '(3 . 7))
                                :d (vector '(2 . 9) '(3 . 9)))))
    
    (assert (finished-p finished-4))
    (assert (finished-p finished-2))

    (loop for letter in *letters*
          do (loop with ary = (letter->positions finished-2 letter)
                   for idx from 0 below (length ary)
                   do (assert (final-place-p finished-2 letter idx))))

    (loop for letter in *letters*
          do (loop with ary = (letter->positions finished-4 letter)
                   for idx from 0 below (length ary)
                   do (assert (final-place-p finished-2 letter idx))))
    ))

;; TODO, keep testing, start with occupied and remaining state methods
(defun test-occupied ()
  (let* ((finished (make-board :a (vector '(2 . 3) '(3 . 3))
                               :b (vector '(2 . 5) '(3 . 5))
                               :c (vector '(2 . 7) '(3 . 7))
                               :d (vector '(2 . 9) '(3 . 9))))
         (occupied (board->occupied finished)))

    (assert (find-item occupied '(2 . 3)))
    (assert (not (find-item occupied '(2 . 1))))

    ))

(defun test-legal-moves-p ()
  (let ((str (list "#############"
                   "#...........#"
                   "###B#C#B#D###"
                   "  #D#C#B#A#"
                   "  #D#B#A#C#"
                   "  #A#D#C#A#"
                   "  #########")))
    (multiple-value-bind (grid the-board) (parse-grid-board str)
      (print-board grid the-board)

      (format t "step 1~%")
      (assert (legal-move-p the-board :d 0 '(1 . 11)))
      (setf the-board (next-board the-board :d 0 '(1 . 11) 3))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 2~%")
      (assert (legal-move-p the-board :a 0 '(1 . 1)))
      (setf the-board (next-board the-board :a 0 '(1 . 1) 10))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 3~%")
      (assert (legal-move-p the-board :b 1 '(1 . 10)))
      (setf the-board (next-board the-board :b 1 '(1 . 10) 4))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 4~%")
      (assert (legal-move-p the-board :b 2 '(1 . 8)))
      (setf the-board (next-board the-board :b 2 '(1 . 8) 2))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 5~%")
      (assert (legal-move-p the-board :a 1 '(1 . 2)))
      (setf the-board (next-board the-board :a 1 '(1 . 2) 8))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 6~%")
      (assert (legal-move-p the-board :c 0 '(4 . 7)))
      (setf the-board (next-board the-board :c 0 '(4 . 7) 6))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 7~%")
      (assert (legal-move-p the-board :c 0 '(3 . 7)))
      (setf the-board (next-board the-board :c 0 '(3 . 7) 6))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 8~%")
      (assert (legal-move-p the-board :d 3 '(1 . 4)))
      (setf the-board (next-board the-board :d 3 '(1 . 4) 5))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 9~%")
      (assert (legal-move-p the-board :b 0 '(5 . 5)))
      (setf the-board (next-board the-board :b 0 '(5 . 5) 5))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 10~%")
      (assert (legal-move-p the-board :b 0 '(4 . 5)))
      (setf the-board (next-board  the-board :b 0 '(4 . 5) 6))
      (print-board grid the-board)
      (format t "~%")

      (format t "step 11~%")
      (assert (legal-move-p the-board :b 0 '(3 . 5)))
      (setf the-board (next-board the-board :b 0 '(3 . 5) 7))
      (print-board grid the-board)
      (format t "~%")
      
      )))

(defun test-cons< ()
  (assert (cons< '(1 . 1) '(2 . 1)))
  (assert (cons< '(1 . 1) '(1 . 2)))
  (assert (not (cons< '(1 . 1) '(1 . 1))))
  (assert (not (cons< '(2 . 1) '(1 . 1))))
  (assert (not (cons< '(1 . 2) '(1 . 1)))))
          
(defun test-target-for-room-p ()
  (let ((the-board (make-board :A #((1 . 1) (1 . 2) (5 . 3) (5 . 9))
                               :B #((1 . 8) (1 . 10) (2 . 3) (4 . 5))
                               :C #((3 . 5) (4 . 7) (4 . 9) (5 . 7))
                               :D #((1 . 11) (3 . 3) (4 . 3) (5 . 5)))))
    
    (assert (legal-move-p the-board :c 0 '(3 . 7)))
))
