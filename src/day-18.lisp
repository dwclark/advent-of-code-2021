(defpackage :day-18
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-18)

(defun expr (str)
  (read-from-string
   (substitute #\space #\, (substitute #\) #\] (substitute #\( #\[ str)))))

(defun day-expressions ()
  (mapcar #'expr (read-day-file "18")))

(defun fish+ (&rest expressions)
  (let* ((e1 (first expressions))
         (e2 (second expressions))
         (working (copy-tree (list e1 e2)))
         (did-explode t)
         (did-split t))
    (loop while (or did-explode did-split)
          do (progn
               (setf (values working did-explode) (explode-expr working))
               (if (not did-explode)
                   (setf (values working did-split) (split-expr working)))))
    (if (= 2 (length expressions))
        working
        (apply #'fish+ (cons working (cddr expressions)))))) 
    
(defun make-path-element (pos current previous)
  (cons (cons pos current) previous))

(defun path-element-pos (path)
  (car path))

(defun path-element-left-p (path)
  (eq :left (path-element-pos path)))

(defun path-element-right-p (path)
  (eq :right (path-element-pos path)))

(defun path-element-expr (path)
  (cdr path))

(defun path-element-left (path)
  (first (path-element-expr path)))

(defun (setf path-element-left) (new-value path)
  (setf (first (cdr path)) new-value)
  path)

(defun path-element-left-snailfish-p (path)
  (consp (path-element-left path)))

(defun path-element-left-regular-p (path)
  (numberp (path-element-left path)))

(defun path-element-right (path)
  (second (path-element-expr path)))

(defun (setf path-element-right) (new-value path)
  (setf (second (cdr path)) new-value)
  path)

(defun path-element-right-snailfish-p (path)
  (consp (path-element-right path)))

(defun path-element-right-regular-p (path)
  (numberp (path-element-right path)))

(defun find-path (tree pred)
  (labels ((iter (rest-tree path)
             (cond ((null rest-tree) nil)
                   ((funcall pred rest-tree path) path)
                   ((consp rest-tree)
                    (or (iter (first rest-tree) (make-path-element :left rest-tree path))
                        (iter (second rest-tree) (make-path-element :right rest-tree path)))))))
    (iter tree nil)))

(defun needs-explode-p (tree)
  (flet ((explode-p (tree path)
           (declare (ignore tree))
           (= 5 (length path))))
    (find-path tree #'explode-p)))

(defun needs-split-p (tree)
  (flet ((split-p (tree path)
           (declare (ignore path))
           (and (numberp tree) (<= 10 tree))))
    (find-path tree #'split-p)))

(defun split-expr (exp)
  (let* ((working (copy-tree exp))
         (path (needs-split-p working)))
    (if path
        (let* ((element (nth 0 path))
               (num (if (path-element-left-p element)
                        (path-element-left element)
                        (path-element-right element)))
               (new-val (list (floor (/ num 2)) (ceiling (/ num 2)))))
          (if (path-element-left-p element)
              (setf (path-element-left element) new-val)
              (setf (path-element-right element) new-val))))
    (values working (if path t nil))))

(defun explode-expr (exp)
  (let* ((working (copy-tree exp))
         (path (needs-explode-p working)))
    
    (labels ((add-first-left (tree num)
               (cond ((null tree) nil)
                     ((numberp (second tree)) (incf (second tree) num))
                     (t (add-first-left (second tree) num))))
             (add-first-right (tree num)
               (cond ((null tree) nil)
                     ((numberp (first tree)) (incf (first tree) num))
                     (t (add-first-right (first tree) num)))))
      
      (if path
          (let ((left-num (path-element-left (nth 0 path)))
                (right-num (path-element-right (nth 0 path)))
                (target-element (nth 1 path)))
            (if (path-element-left-p target-element)
                (progn
                  ;;zero the target
                  (setf (path-element-left target-element) 0)
                  
                  ;;add right
                  (if (path-element-right-regular-p target-element)
                      (incf (path-element-right target-element) right-num)
                      (add-first-right (path-element-right target-element) right-num))
                  
                  ;;add left
                  (let ((new-target (find-if #'path-element-right-p (cdr path))))
                    (if new-target
                        (if (path-element-left-regular-p new-target)
                            (incf (path-element-left new-target) left-num)
                            (add-first-left (path-element-left new-target) left-num)))))

                (progn
                  ;;zero the target
                  (setf (path-element-right target-element) 0)
                  
                  ;;add left
                  (if (path-element-left-regular-p target-element)
                      (incf (path-element-left target-element) left-num)
                      (add-first-left (path-element-left target-element) left-num))
                  
                  ;;add right
                  (let ((new-target (find-if #'path-element-left-p (cdr path))))
                    (if new-target
                        (if (path-element-right-regular-p new-target)
                            (incf (path-element-right new-target) right-num)
                            (add-first-right (path-element-right new-target) right-num)))))))))
      
    (values working (if path t nil))))

(defun magnitude-expr (e)
  (if (numberp e)
      e
      (+ (* 3 (magnitude-expr (first e))) (* 2 (magnitude-expr (second e))))))

(defun test-split ()
  (assert (equal (split-expr (expr "[[[[0,7],4],[15,[0,13]]],[1,1]]"))
                 (expr "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")))
  (assert (equal (split-expr (expr "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"))
                 (expr "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"))))

(defun test-explode ()
  (assert (equal (explode-expr (expr "[[[[[9,8],1],2],3],4]"))
                 (expr "[[[[0,9],2],3],4]")))
  (assert (equal (explode-expr (expr "[7,[6,[5,[4,[3,2]]]]]"))
                 (expr "[7,[6,[5,[7,0]]]]")))
  (assert (equal (explode-expr (expr "[[6,[5,[4,[3,2]]]],1]"))
                 (expr "[[6,[5,[7,0]]],3]")))
  (assert (equal (explode-expr (expr "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
                 (expr "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))
  (assert (equal (explode-expr (expr "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
                 (expr "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))))

(defun test-fish+ ()
  (assert (equal (fish+ (expr "[[[[4,3],4],4],[7,[[8,4],9]]]") (expr "[1,1]"))
                 (expr "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))

  (assert (equal (fish+ (expr "[1,1]") (expr "[2,2]") (expr "[3,3]") (expr "[4,4]"))
                 (expr "[[[[1,1],[2,2]],[3,3]],[4,4]]")))

  (assert (equal (fish+ (expr "[1,1]") (expr "[2,2]") (expr "[3,3]") (expr "[4,4]") (expr "[5,5]"))
                 (expr "[[[[3,0],[5,3]],[4,4]],[5,5]]"))))

(defun test-magnitude-expr ()
  (assert (= 143 (magnitude-expr (expr "[[1,2],[[3,4],5]]"))))
  (assert (= 3488 (magnitude-expr (expr "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")))))
          

(defun part-1 ()
  (magnitude-expr (apply #'fish+ (day-expressions))))

(defun part-2 ()
  (loop with max-sum = 0
        with expressions = (day-expressions)
        for outer in expressions
        do (loop for inner in expressions
                 do (if (not (equal outer inner))
                        (let ((v1 (magnitude-expr (fish+ outer inner)))
                              (v2 (magnitude-expr (fish+ inner outer))))
                          (if (< max-sum v1)
                              (setf max-sum v1))
                          (if (< max-sum v2)
                              (setf max-sum v2)))))
        finally (return max-sum)))
