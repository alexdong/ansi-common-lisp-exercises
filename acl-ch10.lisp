;; 1. If x is a, y is b, and z is (c d), write backquoted expressions containing only variables

(defparameter x 'a)
(defparameter y 'b)
(defparameter z '(c d))

;; which yield the following:

; a) ((C D) A Z)
`(,z ,x z)
; b) (X B C D)
`(x ,y ,@z)
; c) ((C D A) Z)
`((,@z a) z)

;; 2. Define if in terms of cond

(defmacro if-cond (test tclause fclause)
  `(cond (,test ,tclause)
	 (t ,fclause)))

;; 3. Define a macro that takes a number n and any number of expressions and returns the value
;; of the nth expression.

(defmacro nthexpr (n &rest exprs)
  (elt exprs (- n 1)))


;; 4. Re-write the definition of ntimes to use a (local) recursive function.
(defmacro ntimes-r (n &body body)
  `(labels ((rec (i)
	      (when (not (zerop i))
		,@body
		(rec (1- i)))))
     (rec ,n)))

;; 5. Define a macro n-of that takes a number n and an expression and returns a list of n
;; successive values returned by the expression:

;(let ((n 2))
;  (n-of n (incf i)))
;(1 2 3 4)

(defmacro n-of (n expr)
  (let ((g (gensym))
	(h (gensym)))
    `(let ((,g nil))
	(do ((,h ,n (1- ,h)))
	    ((zerop ,h))
	  (push ,expr ,g))
	(nreverse ,g))))

;; 6. Define a macro that takes a list of variables and a body of code, and ensures that
;;the variable revert to their original values after the body of code is evaluated.

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) 
		     `(,s '((gensym))))
		 syms)
     ,@body))

(defmacro revert (&rest vars &body body)
  (with-gensyms vars
    