;; 1. Draw three different trees that would print as ((A) (A) (A)).
;; Write and expression that generates each.

(list (list 'A) (list 'A) (list 'A)) ; simple case

(let ((tree (list (list 'A)))) ; shared structure
  (push (car tree) tree)
  (push (car tree) tree)
  tree)

(let ((tree (list (list 'A)))) ; surely this isn't the third case?
	   (push (list 'A) tree)
	   (push (car tree) tree)
	   tree)

;; 2. Assuming make-queue, enqueue, and dequeue are defined as in 12.7
;; draw the queue in box-notation after each step

; the code from 12.7

(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
	    (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

;; 3. Define a function copy-queue that returns a copy of a queue.

(defun copy-queue (q)
  (let ((cpy (cons nil nil)))
    (setf (car cpy) (copy-list (car q))
	  (cdr cpy) (last (car cpy)))
    cpy))

;; 4. Define a function that takes an object and a queue and puts the object on the
;; front of the queue.

(defun jmp-queue (obj q) ; this is destructive
  (push obj (car q)))

(defun jmp-queue (obj q) ; we can use copy-queue to do it non-destructively
  (push obj (car (copy-queue q))))

;; 5. Define a function that takes an obj and a queue and (destructively) moves the
;; first (eql) instance of the object to the front of the queue.

(defun mv-queue (obj q) ; this is sort of sloppy I think - must be a nicer way?
  (do ((lst (car q) (cdr lst)))
      ((null lst))
    (when (eql obj (cadr lst))
      (push (cadr lst) (car q))
      (setf (cdr lst) (cddr lst))
      (setf lst nil)))
  (setf (cdr q) (last (car q)))
  q)

;; 7. Define a function that returns true when its argument is a cdr-circular list.

(defun cdr-circp (lst)
  (do ((p1 lst (cdr p1))
       (p2 (cdr lst) (cddr p2)))
      ((null p2) nil)
    (print p2)
    (when (eql p1 p2)
      (return t))))
