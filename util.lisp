
(in-package :orcinus-cache)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun mkustr (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
	(format s "~(~a~)" a))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun usymb (&rest args)
    (values (intern (apply #'mkustr args))))

  (defun group (source n)
    (when (zerop n) (error "zero length"))
    (labels ((rec (source acc)
	       (let ((rest (nthcdr n source)))
		 (if (consp rest)
		     (rec rest (cons
				(subseq source 0 n)
				acc))
		   (nreverse
		    (cons source acc))))))
      (when source (rec source nil))))


  (defun g!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "G!"
		  :start1 0
		  :end1 2)))
  (defun o!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "O!"
		  :start1 0
		  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
	  (subseq (symbol-name s) 2)))
  
  (set-dispatch-macro-character #\# #\f
    (lambda (stream sub-char numarg)
      (declare (ignore stream sub-char numarg))
      (setq numarg (or numarg 3))
      (unless (<= numarg 3)
	(error "Bad value for #f: ~a" numarg))
      `(declare (optimize (speed ,numarg)
			  (safety ,(- 3 numarg))
			  (debug 0)
			  (compilation-speed 0)
			  (space 0)))))

  (set-dispatch-macro-character #\# #\d
    (lambda (stream ch n)
      (declare (ignore ch))
      `(progn
	 (format t "~&~@{~A~}"
		 ,@(loop repeat (1- (or n 1)) collect (read stream t nil t)))
	 (prin1 ,(read stream t nil t))))))


(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
		   `(abbrev ,@pair))
	       (group names 2))))

(abbrevs db     destructuring-bind
	 mvb    multiple-value-bind
	 mvsetq multiple-value-setq)

(defmacro in (obj &rest lst)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (x) `(eql ,insym ,x))
		     lst)))))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten body)))))
   `(defmacro ,name ,args
      (let ,(mapcar
	     (lambda (s)
	       `(,s (gensym ,(subseq
			      (symbol-name s)
			      2))))
	     syms)
	,@body))))


(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	  ,(progn ,@body)))))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar (lambda (d)
		   `(,(if (eq t (car d))
			  t
			  (list (car d)))
		      ,(if (eq (cadr d) :const)
			   `(progn ,@(cddr d))
			   `(apply (lambda ,@(cdr d))
				   ,(if (eq t (car d))
					g!args
					`(cdr ,g!args))))))
		 ds))))

(defmacro -> (&rest forms)
  (loop
     with cur = (car forms)
     for form in (cdr forms)
     do
       (let ((vform (if (consp form)
			form
			(list form))))
	 (if (find 'it (flatten vform))
	     (setf cur `(let ((it ,cur))
			  ,form))
	     (let ((x (cons cur (cdr vform))))
	       (rplacd vform x)
	       (setf cur vform))))
     finally
       (return cur)))

(defmacro! ->> (init &rest forms)
  (loop
    with cur = init
    for form in forms
    do (setf cur
	     (if (listp form)
		 (nconc form (list cur))
		 (list form cur)))
    finally (return cur)))

(defmacro! <<- (init &rest forms)
  (loop
    with cur = init
    for form in forms
    do (setf cur
	     (if (listp form)
		 (nconc cur form)
		 (nconc cur (list form))))
    finally (return cur)))


(defmacro! require-params (plist sym-kwd-binding &body body)
  `(let ,(loop
	     for (sym kwd) in (group sym-kwd-binding 2)
	     collect
	       (list sym
		     `(aif (getf ,plist ,kwd)
			   it
			   (error ,(mkstr kwd " must be specified.")))))
     ,@body))


;; (require-params '(:name "Bob" :num 2)
;;     (name :name
;;      num  :num)
;;   (print name))

(defmacro with-gc (&body body)
  `(let ((sb-kernel:*gc-inhibit* nil))
     ,@body))
