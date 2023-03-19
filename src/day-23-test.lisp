(in-package :day-23a)

(defun test-extract-positions ()
  (let* ((test-1 (list "#############"
                       "#...........#"
                       "###A#D#B#C###"
                       "  #B#C#D#A#  "
                       "  #########  "))
         (p-1 (extract-positions test-1))

         (test-2 (list "#############"
                       "#D....B....C#"
                       "###A#.#B#.###"
                       "  #.#C#D#A#  "
                       "  #########  "))
         (p-2 (extract-positions test-2)))

    (assert (equalp (vector 13 29) (gethash :a p-1)))
    (assert (equalp (vector 17 23) (gethash :b p-1)))
    (assert (equalp (vector 19 25) (gethash :c p-1)))
    (assert (equalp (vector 15 27) (gethash :d p-1)))

    (assert (equalp (vector 13 29) (gethash :a p-2)))
    (assert (equalp (vector 6  17) (gethash :b p-2)))
    (assert (equalp (vector 11 25) (gethash :c p-2)))
    (assert (equalp (vector 1  27) (gethash :d p-2)))))


(defun test-pack-positions ()
  (let* ((original (plist-hash-table (list :a (vector 1 2) :b (vector 3 4) :c (vector 5 6) :d (vector 7 8))))
         (packed (pack-positions original)) 
         (unpacked (unpack-positions packed)))
    (format t "~A~%" packed)
    (loop for key in (list :a :b :c :d)
          do (assert (equalp (gethash key original) (gethash key unpacked))))))
    
(defun test-manhattan ()
  (assert (= 10 (manhattan (gethash 11 *nodes-table*) (gethash 23 *nodes-table*))))
  (assert (= 10 (manhattan (gethash 23 *nodes-table*) (gethash 11 *nodes-table*))))
  (assert (= 6 (manhattan (gethash 23 *nodes-table*) (gethash 29 *nodes-table*)))))

(defun test-finished ()
  (let* ((board-1 (list "#############"
                        "#...........#"
                        "###A#D#B#C###"
                        "  #B#C#D#A#  "
                        "  #########  "))
         (not-finished (pack-positions (extract-positions board-1)))
         
         (board-2 (list "#############"
                        "#...........#"
                        "###A#B#C#D###"
                        "  #A#B#C#D#  "
                        "  #########  "))
         (finished (pack-positions (extract-positions board-2))))

    (assert (not (finished-p not-finished)))
    (assert (finished-p finished))))

(defun test-packed->occupied-set ()
  (let* ((the-board (list "#############"
                          "#D....B....C#"
                          "###A#.#B#.###"
                          "  #.#C#D#A#  "
                          "  #########  "))
         (occupied-set (packed->occupied-set (pack-positions (extract-positions the-board)))))
    
    (assert (find-item occupied-set 1))
    (assert (not (find-item occupied-set 2)))
    (assert (find-item occupied-set 25))
    (assert (not (find-item occupied-set 23)))))

(defun test-estimated-remaining-cost ()
  (let* ((finished (extract-positions (list "#############"
                                            "#...........#"
                                            "###A#B#C#D###"
                                            "  #A#B#C#D#  "
                                            "  #########  ")))
         (almost (extract-positions (list "#############"
                                          "#C.........C#"
                                          "###A#B#.#D###"
                                          "  #A#B#.#D#  "
                                          "  #########  "))))
    
    (assert (= 0 (estimated-remaining-cost finished)))
    (assert (= 1300 (estimated-remaining-cost almost)))))

(defun test-final-place-p ()
  (let ((finished (extract-positions (list "#############"
                                           "#...........#"
                                           "###A#B#C#D###"
                                           "  #A#B#C#D#  "
                                           "  #########  ")))
        
        (unfinished (extract-positions (list "#############"
                                             "#.A.........#"
                                             "###.#B#B#D###"
                                             "  #A#C#C#D#  "
                                             "  #########  "))))
    
    (loop for letter being the hash-keys in finished using (hash-value ids)
          do (loop for idx from 0 below (length ids)
                   do (assert (final-place-p letter idx (gethash letter finished)))))

    (assert (not (final-place-p :a 0 (gethash :a unfinished))))
    (assert (final-place-p :a 1 (gethash :a unfinished)))
    (assert (not (final-place-p :b 0 (gethash :b unfinished))))
    (assert (not (final-place-p :b 1 (gethash :b unfinished))))
    (assert (not (final-place-p :c 0 (gethash :c unfinished))))
    (assert (final-place-p :c 1 (gethash :c unfinished)))
    (assert (final-place-p :d 0 (gethash :d unfinished)))
    (assert (final-place-p :d 1 (gethash :d unfinished)))))

(defun test-legal-move-p ()
  (let ((unfinished (extract-positions (list "#############"
                                             "#.A.......BC#"
                                             "###.#.#B#D###"
                                             "  #A#.#C#D#  "
                                             "  #########  "))))

    (assert (not (legal-move-p :a 0 unfinished 1))) ; hall -> hall
    (assert (legal-move-p :a 0 unfinished 13)) ; hall -> room (top of room, with matched room pair)
    (assert (not (legal-move-p :a 0 unfinished 15))) ;hall -> room (tries b room)
    (assert (not (legal-move-p :a 1 unfinished 13))) ; room -> room (tries to move a up to empty square)
    (assert (not (legal-move-p :a 1 unfinished 3))) ; room -> hall (tries to move to cell can't stop on)
    (assert (not (legal-move-p :a 1 unfinished 4))) ; room -> hall (tries to move letter already in final position)
    (assert (legal-move-p :b 0 unfinished 25)) ; hall -> room (tries to move to second cell)
    (assert (not (legal-move-p :b 0 unfinished 15))) ; hall -> room (tries to move to first cell in empty room)
    (assert (not (legal-move-p :d 0 unfinished 8))) ; room -> hall (tries to move cell already in finished position)
    ))

         
