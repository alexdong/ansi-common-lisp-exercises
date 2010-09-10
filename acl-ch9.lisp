;; 1. Define a function which takes a list of reals and returns true iff they are in
;; non-decreasing order.

(defun non-decreasing (reals)
  (not (apply #'> reals)))

;; 2. Define a function that takes an integer number of cents and returns 4 values 
;; showing how to make that number out of 25, 10, 5 and 1 cent pieces, using the
;; smalles number of coins

(defun change (cents)
    (let* ((quarters (truncate (float (/ cents 25))))
	   (dimes (truncate (float (/ (- cents (* quarters 25))
				     10))))
	   (nickels (truncate (float (/ (- cents
					 (* quarters 25)
					 (* dimes 10))
				      5))))
	   (pennies (- cents
		       (* quarters 25)
		       (* dimes 10)
		       (* nickels 5))))
      (values quarters dimes nickels pennies)))
  

;; 3. Define a function that takes 8 reals representing two line segment in 2-space
;; and returns nil if they do not intersect or two values for x and y of their 
;; intersection.

;; There might be a better (faster? easier?) way to do this, but it seemed reasonable
;; to just use high school algebra to solve this problem. Wikipedia indicates that 
;; for the general case of n line segments, the problem is more interesting.

(defun intersect (xa1 ya1
		  xa2 ya2
		  xb1 yb1
		  xb2 yb2)
  (let* ((ma (/ (- ya2 ya1) ; calculate slopes
		(- xa2 xa1)))
	 (mb (/ (- yb2 yb1)
		(- xb2 xb1)))
	 (ca (+ (* ma (- xa1)) ya1)) ; and y-intercepts
	 (cb (+ (* mb (- xb1)) yb1)))
    (if (= ma mb) ; if the lines are parallel then they don't intersect, of course
	nil
	(let* ((xsol (/ (- cb ca)
			(- ma mb))) ; safe from div-by-zero, since slopes /=
	       (ysol (+ (* ma xsol)
			ca)))
	  (if (and (btwn xa1 xa2 xsol) ; check if the solution is on the segments
		   (btwn xb1 xb2 xsol))
	      (values xsol ysol)
	      nil)))))

(defun btwn (a b x)
  (and (<= a x)
       (>= b x)))

