(defpackage :day-23
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :alist-hash-table :define-constant :hash-table-keys :plist-hash-table
                :hash-table-values :copy-hash-table :maphash-keys :rcurry :hash-table-alist)
  (:export #:part-1 #:part-2))

(in-package :day-23)

(defstruct (tracking (:conc-name nil))
  id moves (distance 0))

(define-constant +room-columns+ (list :a 2 :b 4 :c 6 :d 8) :test #'equal)

(defun room-column (s)
  (getf +room-columns+ s))

(define-constant h0 (cons 0 0) :test #'equal)
(define-constant h1 (cons 0 1) :test #'equal)
(define-constant h2 (cons 0 2) :test #'equal)
(define-constant h3 (cons 0 3) :test #'equal)
(define-constant h4 (cons 0 4) :test #'equal)
(define-constant h5 (cons 0 5) :test #'equal)
(define-constant h6 (cons 0 6) :test #'equal)
(define-constant h7 (cons 0 7) :test #'equal)
(define-constant h8 (cons 0 8) :test #'equal)
(define-constant h9 (cons 0 9) :test #'equal)
(define-constant h10 (cons 0 10) :test #'equal)
(define-constant a1 (cons 1 (getf +room-columns+ :a)) :test #'equal)
(define-constant a2 (cons 2 (getf +room-columns+ :a)) :test #'equal)
(define-constant b1 (cons 1 (getf +room-columns+ :b)) :test #'equal)
(define-constant b2 (cons 2 (getf +room-columns+ :b)) :test #'equal)
(define-constant c1 (cons 1 (getf +room-columns+ :c)) :test #'equal)
(define-constant c2 (cons 2 (getf +room-columns+ :c)) :test #'equal)
(define-constant d1 (cons 1 (getf +room-columns+ :d)) :test #'equal)
(define-constant d2 (cons 2 (getf +room-columns+ :d)) :test #'equal)

(defparameter *board*
  (plist-hash-table (list h0 :hall h1 :hall h2 :entry h3 :hall
                          h4 :entry h5 :hall h6 :entry h7 :hall
                          h8 :entry h9 :hall h10 :hall
                          a1 :room a2 :room b1 :room b2 :room
                          c1 :room c2 :room d1 :room d2 :room) :test #'equal))

(defun room-contents (sym track)
  (ecase sym
    (:a (list (gethash a1 track) (gethash a2 track)))
    (:b (list (gethash b1 track) (gethash b2 track)))
    (:c (list (gethash c1 track) (gethash c2 track)))
    (:d (list (gethash d1 track) (gethash d2 track)))))

(defun hallway-p (loc)
  (eq :hall (gethash loc *board*)))

(defun room-p (loc)
  (eq :room (gethash loc *board*)))

(defun legal-from-room (loc track)
  (let ((ret nil))
    (if (and (= 2 (car loc)) (gethash (cons 1 (cdr loc)) track))
        (return-from legal-from-room ret))

    (flet ((add-if-legal (new-loc)
             (if (gethash new-loc track)
                 nil
                 (progn
                   (if (not (eq :entrance (gethash new-loc *board*)))
                       (push new-loc ret))
                   t))))
      
      (loop for h from (1+ (cdr loc)) to 10
            do (let ((new-loc (cons 0 h)))
                 (if (not (add-if-legal new-loc))
                     (return))))

      (loop for h from (1- (cdr loc)) downto 0
            do (let ((new-loc (cons 0 h)))
                 (if (not (add-if-legal new-loc))
                     (return)))))
    ret))

(defun legal-from-hall (loc track)
  (let ((col (cdr loc))
        (dest-col (room-column (id (gethash loc track)))))

    (if (< col dest-col)
        (loop for i from (1+ col) to dest-col
              do (if (gethash (cons 0 i) track)
                     (return-from legal-from-hall nil)))
        (loop for i from (1- col) downto dest-col
              do (if (gethash (cons 0 i) track)
                     (return-from legal-from-hall nil))))

    (if (gethash (cons 1 dest-col) track)
        (return-from legal-from-hall nil))

    (let ((in-cell-2 (gethash (cons 2 dest-col) track)))
      (if (and (not (null in-cell-2))
               (not (eq (id in-cell-2) (id (gethash loc track)))))
          (return-from legal-from-hall nil)))

    ;;should be able to enter a cell now
    (if (not (gethash (cons 2 dest-col) track))
        (list (cons 2 dest-col))
        (list (cons 1 dest-col)))))

(defun legal-moves (loc track)
  (if (final-location-p loc track)
      (return-from legal-moves nil))
  
  (if (= 3 (length (moves (gethash loc track))))
      (return-from legal-moves nil))
  
  (if (hallway-p loc)
      (legal-from-hall loc track)
      (legal-from-room loc track)))

(defun move (loc new-loc track)
  (let* ((ret (make-hash-table :test #'equal))
         (current (gethash loc track))
         (new-inst (copy-tracking current)))
    (setf (distance new-inst) (+ (distance current) (manhattan loc new-loc)))
    (setf (moves new-inst) (cons new-loc (moves new-inst)))
    (flet ((k-v (k v)
             (if (equal k loc)
                 (setf (gethash new-loc ret) new-inst)
                 (setf (gethash k ret) (copy-tracking v)))))
      (maphash #'k-v track))
    ret))

(defun manhattan (p1 p2)
  (+ (abs (- (car p1) (car p2))) (abs (- (cdr p1) (cdr p2)))))

(defun cost (tr)
  (* (distance tr)
     (ecase (id tr)
       (:a 1)
       (:b 10)
       (:c 100)
       (:d 1000))))

(defun total-cost (list-track)
  (reduce #'+ (mapcar #'cost list-track)))

(defun solved-p (track)
  (every (rcurry #'room-correct-p track) (hash-table-keys track)))

(defun room-correct-p (loc track)
  (and (room-p loc)
       (= (cdr loc) (room-column (id (gethash loc track))))))

(defun final-location-p (loc track)
  (let* ((val (gethash loc track))
         (sym (id val))
         (col (room-column sym)))
    (and (= col (cdr loc))
         (or (equal (cons 2 col) loc)
             (let ((other (gethash (cons 2 col) track)))
               (and other (eq sym (id other))))))))

(defvar *solution* most-positive-fixnum)

(defun solve (track)
  (let ((latest-cost (total-cost (hash-table-values track))))
    (if (< *solution* latest-cost)
        (return-from solve nil))
    
    (if (solved-p track)
        (if (< latest-cost *solution*)
            (progn
              (setf *solution* latest-cost)
              (format t "found solution ~A~%" *solution*)))
        (maphash-keys #'(lambda (loc)
                          (let ((moves (legal-moves loc track)))
                                        ;(format t "~A -> ~A~%" loc moves)
                            (loop for new-loc in moves
                                  do (solve (move loc new-loc track)))))
                      track))))

(defun build-tracking (&rest plist)
  (loop with ret = (make-hash-table :test #'equal)
        for (loc sym) on plist by #'cddr
        do (setf (gethash loc ret) (make-tracking :id sym :moves (list loc) :distance 0))
        finally (return ret)))

(defun part-1 ()
  (let ((*solution* most-positive-fixnum))
    (solve (build-tracking a1 :b a2 :a b1 :c b2 :d c1 :b c2 :c d1 :d d2 :a))
    (if *solution*
        (total-cost (hash-table-values *solution*))
        nil)))

(defun test-room-column ()
  (assert (= 4 (room-column :b)))
  (assert (= 8 (room-column :d))))

(defun test-solved-p ()
  (assert (solved-p (build-tracking a1 :a b1 :b)))
  (assert (solved-p (build-tracking a1 :a a2 :a b1 :b b2 :b c1 :c c2 :c d1 :d d2 :d)))
  (assert (not (solved-p (build-tracking a1 :a a2 :a b1 :b b2 :b c1 :c c2 :d d1 :d d2 :d)))))

(defun test-total-cost ()
  (assert (= (+ 10 (* 7 1000) (* 8 100))
             (total-cost (list (make-tracking :id :a :distance 10) (make-tracking :id :d :distance 7)
                               (make-tracking :id :c :distance 8))))))

(defun test-final-location-p ()
  (assert (final-location-p a1 (build-tracking a1 :a a2 :a)))
  (assert (not (final-location-p a1 (build-tracking a1 :a))))
  (assert (not (final-location-p a1 (build-tracking a1 :b a2 :a))))
  (assert (final-location-p a2 (build-tracking a1 :gb a2 :a))))

(defun test-move ()
  (let* ((track (build-tracking a1 :a b1 :b))
         (mov-1 (move a1 h0 track))
         (mov-2 (move b1 h10 mov-1)))
    (assert (not (gethash a1 mov-1)))
    (assert (not (gethash b1 mov-2)))
    (assert (and (gethash h0 mov-2) (gethash h10 mov-2)))
    (assert (= 3 (distance (gethash h0 mov-1))))
    (assert (= 7 (distance (gethash h10 mov-2))))))

(defun matches-locations (moved &rest expected)
  (loop for loc in expected
        do (assert (member loc moved :test #'equal))))

(defun test-legal-room-moves ()
  (let* ((legal-1 (legal-moves a1 (build-tracking a1 :a)))
         (legal-2 (legal-moves a2 (build-tracking a2 :a)))
         (legal-3 (legal-moves a2 (build-tracking a2 :b)))
         (legal-4 (legal-moves d1 (build-tracking d1 :a h7 :d)))
         (legal-5 (legal-moves d1 (build-tracking d1 :a h3 :d)))
         (legal-6 (legal-moves a2 (build-tracking a1 :a a2 :b))))
      
    (matches-locations legal-1 h0 h1 h3 h5 h7 h9 h10)
    (assert (null legal-2))
    (matches-locations legal-3 h0 h1 h3 h5 h7 h9 h10)
    (matches-locations legal-4 h9 h10)
    (matches-locations legal-5 h5 h7 h9 h10)
    (assert (null legal-6))))

(defun test-legal-hall-moves ()
  (matches-locations (legal-moves h0 (build-tracking h0 :a)) a2)
  (matches-locations (legal-moves h0 (build-tracking h0 :a a2 :a)) a1)
  (assert (null (legal-moves h0 (build-tracking a2 :a h0 :a h1 :b))))
  (assert (null (legal-moves h3 (build-tracking h3 :c h5 :d))))
  (matches-locations (legal-moves h5 (build-tracking h5 :c h7 :d)) c2)
  (matches-locations (legal-moves h7 (build-tracking h7 :c h5 :d)) c2)
  (matches-locations (legal-moves h7 (build-tracking h7 :c h5 :d c2 :c)) c1))

(defun test-all ()
  (test-room-column)
  (test-solved-p)
  (test-total-cost)
  (test-final-location-p)
  (test-move)
  (test-legal-room-moves))
