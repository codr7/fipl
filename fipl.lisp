(defpackage fipl
  (:use cl)
  (:export))

(in-package fipl)

(defvar *env*)
(defvar *pos*)
(defvar *forms*)
(defvar *code*)
(defvar *stack*)

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
  (source "*n/a*" :type string)
  (row -1 :type integer)
  (col -1 :type integer))

(defun new-pos (src &key (row 1) (col 1))
  (make-pos :source src :row row :col col))

(defmethod clone ((src pos))
  (copy-structure src))

(defun ws? (c)
  (or (char= c #\space) (char= c #\tab) (char= c #\newline)))

(defun getc (in)
  (read-char in nil))

(defun peekc (in)
  (peek-char nil in nil))

(defun skip-ws (in &key (pos *pos*))
  (labels ((rec (found?)
	     (let ((c (read-char in nil)))
	       (when c
		 (if (ws? c)
		     (progn
		       (case c
			 (#\newline
			  (incf (pos-row pos))
			  (setf (pos-col pos) 1))
			 (otherwise
			  (incf (pos-col pos))))
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

(defun exec (&key (start-pc 0))
  (funcall (aref *code* start-pc)))

(defmethod emit-form ((frm id-form))
  (let* ((id (id-form-id frm))
	 (val (env id)))
    (unless val
      (ecompile (form-pos frm) "Unknown id: ~a" id))
    (let ((pc (length *code*)))
      (emit-op
	(push val *stack*)
	(exec :start-pc (1+ pc))))))

(defstruct (val-form (:include form))
  (val (error "Missing val!") :type t))

(defmethod emit-form ((frm val-form))
  (let ((val (val-form-val frm)))
    (let ((pc (length *code*)))
      (emit-op
	(push val *stack*)
	(exec :start-pc (1+ pc))))))

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))
	  :keyword))

(defun eparse (pos msg &rest args)
  (error (format nil "Parse error in '~a' at row ~a, col ~a: ~a"
		 (pos-source pos) (pos-row pos) (pos-col pos)
		 (apply #'format nil msg args))))

(defun parse-id (in &key (pos *pos*))
  (let ((c (peekc in)))
    (unless (and c (alpha-char-p c))
      (return-from parse-id)))

  (labels ((rec (out)
	     (let ((c (read-char in nil)))
	       (when c
		 (if (sep? c)
		     (unread-char c in)
		     (progn
		       (incf (pos-col pos))
		       (write-char c out)
		       (rec out)))))))
    
    (let ((start-pos (clone pos))
	  (s (with-output-to-string (out)
	       (rec out))))
      (push (make-id-form :pos start-pos :id (kw s)) *forms*)))
  t)

(defun char-digit (c)
  (- (char-code c) (char-code #\0)))

(defun %parse-integer (in &key (pos *pos*))
  (let ((c (peekc in)))
    (unless (and c (digit-char-p c))
      (return-from %parse-integer)))
  
  (labels ((rec (base out)
	     (let ((c (read-char in nil)))
	       (if (and c (digit-char-p c))
		   (progn
		     (incf (pos-col pos))
		     (rec base (+ (* out base) (char-digit c))))
		   (progn
		     (when c
		       (unread-char c in))
		     out)))))
    (let ((start-pos (clone pos)))
      (push (make-val-form :pos start-pos :val (rec 10 0)) *forms*)))
  t)

(defparameter *parsers* (list #'skip-ws #'parse-id #'%parse-integer))

(defun parse (in &key (pos *pos*))
  (labels ((rec ()
	     (when (find-if (lambda (h)
			      (funcall h in :pos pos))
			    *parsers*)
	       (rec))))
    (rec)))

(defun emit (forms)
  (dolist (frm forms) (emit-form frm)))

(defun parse-tests ()
  (let ((*pos* (new-pos "parse-tests")))
    (let-forms ()
      (with-input-from-string (in "foo 42 baz")
	(parse in))
      (assert (= (length *forms*) 3)))))

(defun stack-tests ()
  (let ((*pos* (new-pos "stack-tests")))
    (let-forms ()
      (with-input-from-string (in "42")
	(parse in))
      (let-code ((nreverse *forms*))
	(let-stack ()
	  (exec)
	  (assert (= (pop *stack*) 42)))))))

(defun env-tests ()
  (let ((*pos* (new-pos "emit-tests")))
    (let-forms ()
      (with-input-from-string (in "foo")
	(parse in))
      (let-env (:foo 42)
	(let-code ((nreverse *forms*))
	  (let-stack ()
	    (exec)
	    (assert (= (pop *stack*) 42))))))))

(defun tests ()
  (parse-tests)
  (stack-tests)
  (env-tests))
