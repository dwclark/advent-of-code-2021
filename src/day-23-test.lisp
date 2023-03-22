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
    (assert (finished-p finished-2))))
