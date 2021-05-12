(defpackage fipl
  (:use cl)
  (:export repl tests))

(in-package fipl)

(defparameter *version* 1)

(defvar *pos*)
(defvar *env*)
(defvar *forms*)
(defvar *ops*)
(defvar *stack*)

(define-symbol-macro *source* (pos-row *pos*))
(define-symbol-macro *row* (pos-row *pos*))
(define-symbol-macro *col* (pos-col *pos*))

(defmacro let-env ((&rest in) &body body)
  (labels ((rec (in out)
	     (if in
		 (let ((k (pop in)) (v (pop in)))
		   (rec in (cons `(setf (env ,k) ,v) out)))
		 (nreverse out))))
    
    `(let ((*env* (make-hash-table :test 'eq)))
       ,@(rec in nil)
       ,@body)))

(defmacro let-stack ((&optional in) &body body)
  `(let ((*stack* (or ,in (make-array 0 :adjustable t :fill-pointer 0))))
     ,@body))

(defun eparse (msg &rest args)
  (error (format nil "Parse error in '~a' at row ~a, col ~a: ~a"
		 *source* *row* *col*
		 (apply #'format nil msg args))))

(defun ecompile (msg &rest args)
  (error (format nil "Compile error in '~a' at row ~a, col ~a: ~a"
		 *source* *row* *col*
		 (apply #'format nil msg args))))

(defun erun (msg &rest args)
  (error (format nil "Run error in '~a' at row ~a, col ~a: ~a"
		 *source* *row* *col*
		 (apply #'format nil msg args))))

(defstruct pos
  (source (error "Missing source") :type string)
  (row (error "Missing row") :type integer)
  (col (error "Missing col") :type integer))

(defun new-pos (src &key (row 1) (col 1))
  (make-pos :source src :row row :col col))

(defparameter *nil-pos* (new-pos "n/a" :row -1 :col -1))

(defmethod clone-pos ()
  (copy-structure *pos*))

(defmacro let-pos ((src &key (row 1) (col 1)) &body body)
  `(let ((*pos* (new-pos ,src :row ,row :col ,col)))
     ,@body))

(defparameter *na* (gensym))

(defun env (key)
  (gethash key *env* *na*))

(defun (setf env) (val key)
  (setf (gethash key *env*) val))

(defun ws? (c)
  (or (char= c #\space)
      (char= c #\tab)
      (char= c #\newline)))

(defun getc (in)
  (read-char in nil))

(defun peekc (in)
  (peek-char nil in nil))

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

(defparameter *nil-form* (make-form :pos *nil-pos*))

(defmacro let-forms ((&optional in) &body body)
  `(let ((*forms* ,in))
     ,@body))

(defmacro let-ops ((forms) &body body)
  `(let ((*ops* nil))
     (compile-forms ,forms)
     (compile-op (*nil-form*))

     (let ((*ops* (make-array (length *ops*)
			      :initial-contents (nreverse *ops*)
			      :element-type 'function)))
       ,@body)))

(defstruct (id-form (:include form))
  (id (error "Missing id") :type keyword))

(defmacro compile-op ((frm) &body body)
  `(push (lambda () (let ((*pos* (form-pos ,frm))) ,@body)) *ops*))

(defun exec (&key (start 0))
  (funcall (aref *ops* start)))

(define-symbol-macro *pc*
    (1+ (length *ops*)))

(defmethod deref-val (val frm pc ref?)
  (cond 
    ((and (functionp val) (not ref?))
     (compile-op (frm)
       (funcall val)
       (exec :start pc)))
    (t
     (compile-op (frm)
       (push-val val)
       (exec :start pc)))))
  
(defmethod compile-form ((frm id-form))
  (let* ((id (id-form-id frm))
	 (idn (symbol-name id))
	 (ref? (char= #\& (char idn 0)))
	 (val (env (if ref? (kw (subseq idn 1)) id)))
	 (*pos* (form-pos frm)))
    (when (eq val *na*)
      (ecompile "Unknown id: ~a" id))
    (deref-val val frm *pc* ref?)))

(defstruct (val-form (:include form))
  (val (error "Missing val") :type t))

(defmethod compile-form ((frm val-form))
  (let ((val (val-form-val frm))
	(pc *pc*)
	(*pos* (form-pos frm)))
      (compile-op (frm)
	(push-val val)
	(exec :start pc))))

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))
	  :keyword))

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
      (push (make-id-form :pos start-pos
			  :id (kw s))
	    *forms*)))
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
      (push (make-val-form :pos start-pos
			   :val (rec 10 0))
	    *forms*)))
  t)

(defparameter *parsers*
  (list #'skip-ws #'parse-id #'%parse-integer))

(defun parse (in)
  (labels ((rec ()
	     (when (find-if (lambda (h)
			      (funcall h in))
			    *parsers*)
	       (rec))))
    (rec)))

(defun compile-forms (forms)
  (dolist (frm forms) (compile-form frm)))

(defmethod dump-val (val out)
  (print-object val out))

(defmethod dump-val ((val (eql t)) out)
  (write-char #\t out))

(defmethod dump-val ((val (eql nil)) out)
  (write-char #\f out))

(defun dump-stack (&optional (out *standard-output*))
  (write-char #\[ out)
  (dotimes (i (length *stack*))
    (when (not (zerop i))
      (write-char #\space out))
    (dump-val (aref *stack* i) out))
  (write-char #\] out))

(defun push-val (val)
  (vector-push-extend val *stack*))

(defun pop-val ()
  (vector-pop *stack*))

(defun d ()
  (pop-val))

(defun cp ()
  (push-val (aref *stack* 0)))

(defun swap ()
  (rotatef (aref *stack* 0) (aref *stack* 1)))

(defun rotl ()
  (rotatef (aref *stack* 0)
	   (aref *stack* 2)
	   (aref *stack* 1)))

(defun rotr ()
  (rotatef (aref *stack* 0)
	   (aref *stack* 1)
	   (aref *stack* 2)))

(defun call ()
  (funcall (pop-val)))

(defun repl ()
  (format t "Welcome to fipl v~a~%~%Press Return twice to evaluate~%~%" *version*)
  
  (let-env (:t t
	    :f nil
	    
	    :d #'d
	    :cp #'cp
	    :swap #'swap
	    :rotl #'rotl
	    :rotr #'rotr
	    
	    :call #'call)
    (let-stack ()
      (labels ((read-next ()
		 (let ((st (with-output-to-string (out)
			     (labels ((read-ln ()
					(write-string "  ")
					(force-output)
					
					(let ((ln (read-line)))
					  (unless (string= ln "")
					    (write-string ln out)
					    (read-ln)))))
			       (read-ln)))))
		   (let-pos ("repl")
		     (let-forms ()
		       (with-input-from-string (sts st)
			 (parse sts))
		       (let-ops ((nreverse *forms*))
			 (exec)
			 (dump-stack)
			 (terpri)
			 (read-next)))))))
      (read-next)))))
