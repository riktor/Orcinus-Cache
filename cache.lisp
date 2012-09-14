
(in-package :orcinus-cache)

(defun get-fill-mask (size)
  (make-array size
	      :initial-element 1
	      :element-type 'bit))

(defun get-empty-mask (size)
  (make-array size
	      :initial-element 0
	      :element-type 'bit))

(defun gen-funcname-sym (&rest args)
  (funcall (lambda (args) (apply #'usymb args)) args))

(defmacro! defcache (prefix &body params-plist)
  (require-params params-plist
      (ctype     :cache-type
       csize     :cache-size
       src-fn    :src-function)
    (unless (constantp csize)
      (error "Cache size must be a constant fixnum."))
    (db (init-fn get-mac set-mac rem-mac clr-mac)
      (case ctype
	(:lru '(lru-init lru-get lru-set lru-rem lru-clr))
	(t (error "Unknown cache type.")))
      `(labels
	   ((,g!def (getsym setsym remsym clrsym)
	      (let ((,g!cache      (,init-fn ,csize))
		    (,g!fill  ,(get-fill-mask csize))
		    (,g!empty ,(get-empty-mask csize)))
		(setf (symbol-function getsym)
		      (lambda (key &rest args)
			,(macroexpand
			  `(,get-mac key ,src-fn args
				     ,g!fill ,g!empty ,g!cache))))
		(setf (symbol-function setsym)
		      (lambda (key val)
			,(macroexpand
			  `(,set-mac key val
				     ,g!fill ,g!empty ,g!cache))))
		(setf (symbol-function remsym)
		      (lambda (key)
			,(macroexpand
			  `(,rem-mac key ,g!empty ,g!cache))))
		(setf (symbol-function clrsym)
		      (lambda ()
			,(macroexpand
			  `(,clr-mac ,g!empty ,g!cache)))))))
	 (let ((getsym (gen-funcname-sym ',prefix "-get"))
	       (setsym (gen-funcname-sym ',prefix "-set"))
	       (remsym (gen-funcname-sym ',prefix "-rem"))
	       (clrsym (gen-funcname-sym ',prefix "-clr")))
	   (,g!def getsym setsym remsym clrsym))))))


