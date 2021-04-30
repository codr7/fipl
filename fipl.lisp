(defpackage fipl
  (:use cl)
  (:export))

(in-package fipl)

(defvar *env* (make-hash-table :test 'eq))
(defvar *form*)
(defvar *parser*)
(defvar *pos*)
(defvar *code*)
(defvar *stack*)

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

(defclass parser ()
  ((handlers :initform nil :initarg :handlers :accessor handlers)
   (forms :initform nil :initarg :forms :accessor forms)))

(defun ws? (c)
  (when (or (char= c #\space) (char= c #\tab) (char= c #\newline))
    c))

(defun getc (in)
  (read-char in nil))

(defun peekc (in)
  (peek-char nil in nil))

(defun skip-ws (in &key (parser *parser*) (pos *pos*))
  (declare (ignore parser))
  
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
  (id nil :type keyword))

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

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))
	  :keyword))

(defun eparse (pos msg &rest args)
  (error (format nil "Parse error in '~a' at row ~a, col ~a: ~a"
		 (pos-source pos) (pos-row pos) (pos-col pos)
		 (apply #'format nil msg args))))

(defun parse-id (in &key (parser *parser*) (pos *pos*))
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
      (push (make-id-form :pos start-pos :id (kw s)) (forms parser))))

  t)

(defun new-parser ()
  (let ((p (make-instance 'parser)))
    (push #'parse-id (handlers p))
    (push #'skip-ws (handlers p))
    p))

(defun parse (in &key (parser *parser*) (pos *pos*))
  (tagbody
   next
     (when (find-if (lambda (h)
		      (funcall h in :parser parser :pos pos))
		    (handlers parser))
       (go next))))

(defun emit (forms)
  (dolist (frm forms) (emit-form frm)))

(defun parse-tests ()
  (let ((*parser* (new-parser))
	(*pos* (new-pos "parse-tests")))
    (with-input-from-string (in "foo bar baz")
      (parse in))
    (assert (= (length (forms *parser*)) 3))))

(defun env-tests ()
  (let ((*parser* (new-parser))
	(*pos* (new-pos "emit-tests")))
    (with-input-from-string (in "foo")
      (parse in))
    (setf (env :foo) 42)
    (let-code ((nreverse (forms *parser*)))
      (let-stack ()
	(exec)
	(assert (= (pop *stack*) 42))))))

(defun tests ()
  (parse-tests)
  (env-tests))
