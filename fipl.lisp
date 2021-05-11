(defpackage fipl
  (:use cl)
  (:export))

(in-package fipl)

(defvar *env*)
(defvar *pos*)
(defvar *forms*)
(defvar *code*)
(defvar *stack*)

(defmacro let-pos ((src &key (row 1) (col 1)) &body body)
  `(let ((*pos* (new-pos ,src :row ,row :col ,col)))
     ,@body))

(defmacro let-forms ((&optional in) &body body)
  `(let ((*forms* ,in))
     ,@body))

(defmacro let-env ((&rest in) &body body)
  (labels ((rec (in out)
	     (if in
		 (let ((k (pop in)) (v (pop in)))
		   (rec in (cons `(setf (env ,k) ,v) out)))
		 (nreverse out))))
    
    `(let ((*env* (make-hash-table :test 'eq)))
       ,@(rec in nil)
       ,@body)))

(defmacro let-code ((forms) &body body)
  `(let ((*code* nil))
     (emit ,forms)
     (emit-op)

     (let ((*code* (make-array (length *code*) :initial-contents (nreverse *code*) :element-type 'function)))
       ,@body)))

(defmacro let-stack ((&optional in) &body body)
  `(let ((*stack* ,in))
     ,@body))

(defun env (key)
  (gethash key *env*))

(defun (setf env) (val key)
  (setf (gethash key *env*) val))

(defstruct pos
  (source (error "Missing source!") :type string)
  (row (error "Missing row!") :type integer)
  (col (error "Missing col!") :type integer))

(defun new-pos (src &key (row 1) (col 1))
  (make-pos :source src :row row :col col))

(defmethod clone-pos ()
  (copy-structure *pos*))

(defun ws? (c)
  (or (char= c #\space) (char= c #\tab) (char= c #\newline)))

(defun getc (in)
  (read-char in nil))

(defun peekc (in)
  (peek-char nil in nil))

(define-symbol-macro *row*
    (pos-row *pos*))

(define-symbol-macro *col*
    (pos-col *pos*))

(defun skip-ws (in)
  (labels ((rec (found?)
	     (let ((c (read-char in nil)))
	       (when c
		 (if (ws? c)
		     (progn
		       (case c
			 (#\newline
			  (incf *row*)
			  (setf *col* 1))
			 (otherwise
			  (incf *col*)))
		       (rec t))
		     (unread-char c in))))
	     found?))
    (rec nil)))

(defun sep? (c)
  (or (ws? c) (char= c #\( #\))))

(defstruct form
  (pos nil :type pos))

(defstruct (id-form (:include form))
  (id (error "Missing id!") :type keyword))

(defun ecompile (pos msg &rest args)
  (error (format nil "Compile error in '~a' at row ~a, col ~a: ~a"
		 (pos-source pos) (pos-row pos) (pos-col pos)
		 (apply #'format nil msg args))))

(defmacro emit-op (&body body)
  `(push (lambda () ,@body) *code*))

(defun exec (&key (start 0))
  (funcall (aref *code* start)))

(define-symbol-macro *pc*
    (1+ (length *code*)))

(defmethod emit-form ((frm id-form))
  (let* ((id (id-form-id frm))
	 (idn (symbol-name id))
	 (ref? (char= #\& (char idn 0)))
	 (val (env (if ref? (kw (subseq idn 1)) id))))
    (unless val
      (ecompile (form-pos frm) "Unknown id: ~a" id))
    (let ((pc *pc*))
      (cond 
	((and (functionp val) (not ref?))
	 (emit-op
	   (funcall val)
	   (exec :start pc)))
	(t
	 (emit-op
	   (push val *stack*)
	   (exec :start pc)))))))

(defstruct (val-form (:include form))
  (val (error "Missing val!") :type t))

(defmethod emit-form ((frm val-form))
  (let ((val (val-form-val frm)))
    (let ((pc *pc*))
      (emit-op
	(push val *stack*)
	(exec :start pc)))))

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))
	  :keyword))

(defun eparse (pos msg &rest args)
  (error (format nil "Parse error in '~a' at row ~a, col ~a: ~a"
		 (pos-source pos) (pos-row pos) (pos-col pos)
		 (apply #'format nil msg args))))

(defun parse-id (in)
  (let ((c (peekc in)))
    (when (or (not c) (sep? c) (digit-char-p c))
      (return-from parse-id)))

  (labels ((rec (out)
	     (let ((c (read-char in nil)))
	       (when c
		 (if (sep? c)
		     (unread-char c in)
		     (progn
		       (incf *col*)
		       (write-char c out)
		       (rec out)))))))
    
    (let ((start-pos (clone-pos))
	  (s (with-output-to-string (out)
	       (rec out))))
      (push (make-id-form :pos start-pos :id (kw s)) *forms*)))
  t)

(defun char-digit (c)
  (- (char-code c) (char-code #\0)))

(defun %parse-integer (in)
  (let ((c (peekc in)))
    (unless (and c (digit-char-p c))
      (return-from %parse-integer)))
  
  (labels ((rec (base out)
	     (let ((c (read-char in nil)))
	       (if (and c (digit-char-p c))
		   (progn
		     (incf *col*)
		     (rec base (+ (* out base) (char-digit c))))
		   (progn
		     (when c
		       (unread-char c in))
		     out)))))
    (let ((start-pos (clone-pos)))
      (push (make-val-form :pos start-pos :val (rec 10 0)) *forms*)))
  t)

(defparameter *parsers* (list #'skip-ws #'parse-id #'%parse-integer))

(defun parse (in)
  (labels ((rec ()
	     (when (find-if (lambda (h)
			      (funcall h in))
			    *parsers*)
	       (rec))))
    (rec)))

(defun emit (forms)
  (dolist (frm forms) (emit-form frm)))

(defun d ()
  (pop *stack*))

(defun cp ()
  (push (first *stack*) *stack*))

(defun swap ()
  (rotatef (first *stack*) (second *stack*)))

(defun rotl ()
  (rotatef (first *stack*) (third *stack*) (second *stack*)))

(defun rotr ()
  (rotatef (first *stack*) (second *stack*) (third *stack*)))

(defun call ()
  (funcall (pop *stack*)))

(defun parse-tests ()
  (let-pos ("parse-tests")
    (let-forms ()
      (with-input-from-string (in "foo 42 baz")
	(parse in))
      (assert (= (length *forms*) 3)))))

(defun stack-tests ()
  (let-pos ("stack-tests")
    (let-forms ()
      (with-input-from-string (in "42")
	(parse in))
      (let-code ((nreverse *forms*))
	(let-stack ()
	  (exec)
	  (assert (= (pop *stack*) 42)))))))

(defun env-tests ()
  (let-pos ("env-tests")
    (let-forms ()
      (with-input-from-string (in "foo")
	(parse in))
      (let-env (:foo 42)
	(let-code ((nreverse *forms*))
	  (let-stack ()
	    (exec)
	    (assert (= (pop *stack*) 42))))))))

(defun fn-tests ()
  (let-pos ("fn-tests")
    (let-forms ()
      (with-input-from-string (in "42 cp")
	(parse in))
      (let-env (:cp #'cp)
	(let-code ((nreverse *forms*))
	  (let-stack ()
	    (exec)
	    (assert (= (pop *stack*) 42))
	    (assert (= (pop *stack*) 42))))))))

(defun ref-tests ()
  (let-pos ("ref-tests")
    (let-forms ()
      (with-input-from-string (in "42 &cp call")
	(parse in))
      (let-env (:cp #'cp :call #'call)
	(let-code ((nreverse *forms*))
	  (let-stack ()
	    (exec)
	    (assert (= (pop *stack*) 42))
	    (assert (= (pop *stack*) 42))))))))

(defun tests ()
  (parse-tests)
  (stack-tests)
  (env-tests)
  (fn-tests)
  (ref-tests))
