;; 1. Define a function that takes  filename and returns a list of the lines
;; in the file.

(defun file->lst-of-lines (file)
  (file->lst file #'read-line))

(defun file->lst (file reader)
  (with-open-file (str file :direction :input)
    (let ((retval nil))
      (do ((line (funcall reader str nil :eof)
		 (funcall reader str nil :eof)))
	  ((eql line :eof))
	(push line retval))
      retval)))

;; 2. Define a function that takes a filename and returns a list of the
;; expressions in the file.

(defun file->lst-of-expr (file)
  (file->lst file #'read))

;; 3. Define a function that removes comments from a file and writes the result
;; to a second file. Suppose that comments start with '%' and run to the end of
;; the line.

(defun remove-%comments (in out)
  (with-open-file (instr in :direction :input)
    (with-open-file (outstr out :direction :output :if-exists :supersede)
      (let ((seen nil))
	(do ((c (read-char instr nil :eof)
		(read-char instr nil :eof)))
	    ((eql c :eof))
	  (cond (seen (when (eql c #\newline)
			(princ c outstr)
			(setf seen nil)))
		((not seen)
		 (if (eql c #\%)
		     (setf seen t)
		     (princ c outstr)))))))))

;; 4. Define a function that takes a two dimensional array of floats and 
;; displays it in neat columns. Each element should be printed with two digits
;; after the decimal point in a field.

