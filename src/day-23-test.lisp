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

(defun test-occupied-p ()
  (let* ((the-board (list "#############"
                          "#D....B....C#"
                          "###A#.#B#.###"
                          "  #.#C#D#A#  "
                          "  #########  "))
         (packed (pack-positions (extract-positions the-board))))

    (assert (occupied-p 1 packed))
    (assert (not (occupied-p 2 packed)))
    (assert (occupied-p 25 packed))
    (assert (not (occupied-p 23 packed)))))

(defun test-estimated-remaining-cost ()
  (let* ((finished (list "#############"
                         "#...........#"
                         "###A#B#C#D###"
                         "  #A#B#C#D#  "
                         "  #########  "))
         (packed-finished (pack-positions (extract-positions finished)))

         (almost (list "#############"
                       "#C.........C#"
                       "###A#B#.#D###"
                       "  #A#B#.#D#  "
                       "  #########  "))

         (packed-almost (pack-positions (extract-positions almost))))
    
    (assert (= 0 (estimated-remaining-cost packed-finished)))
    (assert (= 1300 (estimated-remaining-cost packed-almost)))))
