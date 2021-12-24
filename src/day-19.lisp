(defpackage :day-19
  (:use :cl)
  (:import-from :utils :read-blank-line-blocks)
  (:import-from :cl-ppcre :split :do-register-groups)
  (:import-from :alexandria :hash-table-alist :curry)
  (:import-from :metabang.cl-containers :basic-queue :insert-item :delete-first :empty-p)
  (:export #:part-1 #:part-2))

(in-package :day-19)

(defun get-x (coord)
  (aref coord 0))

(defun get-y (coord)
  (aref coord 1))

(defun get-z (coord)
  (aref coord 2))

(defun roll (coord)
  (vector (get-x coord) (get-z coord) (- (get-y coord))))

(defun turn (coord)
  (vector (- (get-y coord)) (get-x coord) (get-z coord)))

(defun all-transforms (coord)
  (loop with v = coord
        with all = nil
        for cycle from 0 to 1
        do (loop for step from 0 to 2
                 do (progn
                      (setf v (roll v))
                      (push v all)
                      (loop for i from 0 to 2
                            do (progn
                                 (setf v (turn v))
                                 (push v all))))
                 finally (setf v (roll (turn (roll v)))))
        finally (return all)))

(defparameter *all-transforms* 
  (list #'(lambda (c) (vector (- (get-z c)) (- (get-y c)) (- (get-x c))))
        #'(lambda (c) (vector (- (get-y c)) (get-z c) (- (get-x c))))
        #'(lambda (c) (vector (get-z c) (get-y c) (- (get-x c))))
        #'(lambda (c) (vector (get-y c) (- (get-z c)) (- (get-x c))))
        #'(lambda (c) (vector (get-y c) (get-x c) (- (get-z c))))
        #'(lambda (c) (vector (get-x c) (- (get-y c)) (- (get-z c))))
        #'(lambda (c) (vector (- (get-y c)) (- (get-x c)) (- (get-z c))))
        #'(lambda (c) (vector (- (get-x c)) (get-y c) (- (get-z c))))
        #'(lambda (c) (vector (- (get-x c)) (get-z c) (get-y c)))
        #'(lambda (c) (vector (get-z c) (get-x c) (get-y c)))
        #'(lambda (c) (vector (get-x c) (- (get-z c)) (get-y c)))
        #'(lambda (c) (vector (- (get-z c)) (- (get-x c)) (get-y c)))
        #'(lambda (c) (vector (get-x c) (get-y c) (get-z c)))
        #'(lambda (c) (vector (get-y c) (- (get-x c)) (get-z c)))
        #'(lambda (c) (vector (- (get-x c)) (- (get-y c)) (get-z c)))
        #'(lambda (c) (vector (- (get-y c)) (get-x c) (get-z c)))
        #'(lambda (c) (vector (- (get-y c)) (- (get-z c)) (get-x c)))
        #'(lambda (c) (vector (- (get-z c)) (get-y c) (get-x c)))
        #'(lambda (c) (vector (get-y c) (get-z c) (get-x c)))
        #'(lambda (c) (vector (get-z c) (- (get-y c)) (get-x c)))
        #'(lambda (c) (vector (get-z c) (- (get-x c)) (- (get-y c))))
        #'(lambda (c) (vector (- (get-x c)) (- (get-z c)) (- (get-y c))))
        #'(lambda (c) (vector (- (get-z c)) (get-x c) (- (get-y c))))
        #'(lambda (c) (vector (get-x c) (get-z c) (- (get-y c))))))

(defparameter *all-inverse-transforms*
  (mapcar #'(lambda (f)
              (find-if #'(lambda (inv)
                           (equalp (vector 1 2 3) (funcall inv (funcall f (vector 1 2 3))))) *all-transforms*)) *all-transforms*))

(defstruct scanner index coordinates)

(defun make-scanners ()
  (loop with ret = nil
        with strs = (read-blank-line-blocks "19")
        for str in strs
        do (let ((lines (split "\\n+" str)))
             (do-register-groups ((#'parse-integer index)) ("--- scanner (\\d+)" (first lines))
               (loop with coords = nil
                     for coord-str in (rest lines)
                     do (do-register-groups ((#'parse-integer x) (#'parse-integer y) (#'parse-integer z)) ("(-?\\d+),(-?\\d+),(-?\\d+)" coord-str)
                          (push (vector x y z) coords))
                     finally (push (make-scanner :index index :coordinates (reverse coords)) ret))))
        finally (return (reverse ret))))

(defun coord- (c1 c2)
  (vector (- (get-x c1) (get-x c2))
          (- (get-y c1) (get-y c2))
          (- (get-z c1) (get-z c2))))

(defun coord+ (c1 c2)
  (vector (+ (get-x c1) (get-x c2))
          (+ (get-y c1) (get-y c2))
          (+ (get-z c1) (get-z c2))))

(defun manhattan (c1 c2)
  (reduce #'+ (vector (abs (- (get-x c1) (get-x c2)))
                      (abs (- (get-y c1) (get-y c2)))
                      (abs (- (get-z c1) (get-z c2))))))


;;finds t = func(source) - dest
(defun find-translation (func source-coordinates dest-coordinates)
  (loop with table = (make-hash-table :test #'equalp)
        for transformed-source in (mapcar func source-coordinates)
        do (loop for dest in dest-coordinates
                 do (incf (gethash (coord- transformed-source dest) table 0))
                 finally (loop for key being the hash-keys in table using (hash-value val)
                               do (if (<= 12 val)
                                      (return-from find-translation key)))))
  nil)

(defun find-transform-and-translation (source-scanner dest-scanner)
  (loop with source-coordinates = (scanner-coordinates source-scanner)
        with dest-coordinates = (scanner-coordinates dest-scanner)
        for idx from 0 below (length *all-transforms*)
        do (let* ((func (nth idx *all-transforms*))
                  (translation (find-translation func source-coordinates dest-coordinates)))
             (if translation
                 (return-from find-transform-and-translation (values idx translation)))))
  (values nil nil))

;;TODO inverse translation does not appear to work.
;;since t = func(source) - dest, then dest = func(source) - t
(defun build-all-transformations (scanners)
  (loop with table = (make-hash-table :test #'equal)
        for outer-idx from 0 below (length scanners)
        do (loop with source-scanner = (nth outer-idx scanners)
                 for inner-idx from (1+ outer-idx) below (length scanners)
                 do (let ((dest-scanner (nth inner-idx scanners)))
                      (multiple-value-bind (forward-idx forward-translation) (find-transform-and-translation source-scanner dest-scanner)
                        (if forward-translation
                            (multiple-value-bind (reverse-idx reverse-translation) (find-transform-and-translation dest-scanner source-scanner)
                              (let ((forward-func (nth forward-idx *all-transforms*))
                                    (reverse-func (nth reverse-idx *all-transforms*)))
                                ;;forward translation
                                (setf (gethash (cons outer-idx inner-idx) table)
                                      #'(lambda (coord)
                                          (coord- (funcall forward-func coord) forward-translation)))
                                ;;inverse translation
                                (setf (gethash (cons inner-idx outer-idx) table)
                                      #'(lambda (coord)
                                          (coord- (funcall reverse-func coord) reverse-translation)))))))))
           
        finally (return table)))

(defun shortest-path (initial goal-func neighbors-func)
  (let ((queue (make-instance 'basic-queue))
        (visited (make-hash-table)))
    (insert-item queue (list initial))

    (loop while (not (empty-p queue))
          do (let* ((path (delete-first queue))
                    (current (car path)))
               (if (funcall goal-func current)
                   (return-from shortest-path (reverse path)))
               
               (setf (gethash current visited) t)
               (loop for child in (funcall neighbors-func current)
                     do (if (not (gethash child visited))
                            (insert-item queue (cons child path))))))
    nil))

(defun apply-all-transformations (table scanner path)
  (loop with working = (scanner-coordinates scanner)
        with current-idx = (first path)
        for idx in (rest path)
        do (let ((func (gethash (cons current-idx idx) table)))
             (setf working (mapcar func working))
             (setf current-idx idx))
        finally (return working)))

(defun origin-transformation (table path)
  (loop with working = (vector 0 0 0)
        with current-idx = (first path)
        for idx in (rest path)
        do (let ((func (gethash (cons current-idx idx) table)))
             (setf working (funcall func working))
             (setf current-idx idx))
        finally (return working)))

(defun all-paths->zero (scanners all-paths)
  (flet ((neighbors (x)
           (mapcar #'cdr (remove-if-not (curry #'= x) all-paths :key #'car))))
    (loop with paths = (make-hash-table :test #'equalp)
          for scanner in (rest scanners)
          do (setf (gethash (scanner-index scanner) paths)
                   (shortest-path (scanner-index scanner) (curry #'= 0) #'neighbors))
          finally (return paths))))

(defun part-1 ()
  (let* ((scanners (make-scanners))
         (transform-table (build-all-transformations scanners))
         (alist (hash-table-alist transform-table))
         (all-paths (mapcar #'car alist))
         (paths->zero (all-paths->zero scanners all-paths))
         (de-duped (make-hash-table :test #'equalp)))

    ;; add in rest of scanners
    (loop for scanner in (rest scanners)
          do (let ((path (gethash (scanner-index scanner) paths->zero)))
               (loop for coord in (apply-all-transformations transform-table scanner path)
                     do (setf (gethash coord de-duped) t))))

    ;;add in scanner 0
    (dolist (coord (scanner-coordinates (first scanners)))
      (setf (gethash coord de-duped) t))
    
    (hash-table-count de-duped)))


(defun part-2 ()
  (let* ((scanners (make-scanners))
         (transform-table (build-all-transformations scanners))
         (alist (hash-table-alist transform-table))
         (all-paths (mapcar #'car alist))
         (paths->zero (all-paths->zero scanners all-paths))
         (all-origins nil))
    (push (vector 0 0 0) all-origins)
    (loop for scanner in (rest scanners)
          do (push (origin-transformation transform-table (gethash (scanner-index scanner) paths->zero)) all-origins))

    (loop with max-distance = 0
          for outer-idx from 0 below (length all-origins)
          do (loop with outer-origin = (nth outer-idx all-origins)
                   for inner-idx from 1 below (length all-origins)
                   do (let* ((inner-origin (nth inner-idx all-origins))
                             (distance (manhattan outer-origin inner-origin)))
                        (if (< max-distance distance)
                            (setf max-distance distance))))
          finally (return max-distance))))
                        
          
