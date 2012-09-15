
(in-package :orcinus-cache)

(defclass lru-cache ()
  ((size :type fixnum
	 :initarg :csize
	 :reader csize)
   (fmsk :initarg :fmsk
	 :reader fmsk)
   (emsk :initarg :emsk
	 :reader emsk)
   (mat  :initarg :mat
	 :reader mat)
   (aval :initarg :aval
	 :reader aval)
   (ht   :initarg :ht
	 :reader ht)
   (iht  :initarg :iht
	 :reader iht)
   (default-src-fn
       :initarg :dsrcfn
       :reader dsrcfn)
   (default-windup-fn
       :initarg :dwindfn
       :reader dwindfn)))

(defmethod print-object ((inst lru-cache) stream)
  (print-unreadable-object (inst stream :type t)
    (format stream "~%size: ~s~%mat : ~s~%aval: ~s"
	    (csize inst) (mat inst) (aval inst))))

(defmacro lru-init (size src-fn windup-fn &key vtype (test 'equal))
  (unless (constantp size)
       (error "Cache size must be a constant fixnum."))
  (let ((chc (make-array size
			 :element-type `(simple-bit-vector ,size))))
    (dotimes (i size)
      (setf (aref chc i)
	    (make-array size
			:element-type 'bit)))
    `(make-instance 'lru-cache
		    :csize ,size
		    :fmsk (make-array ,size
				      :initial-element 1
				      :element-type 'bit)
		    :emsk (make-array ,size
				      :initial-element 0
				      :element-type 'bit)
		    :mat  ,chc
		    :aval (if ,vtype
			      (make-array ,size :element-type ,vtype)
			      (make-array ,size :initial-element nil))
		    :ht  (make-hash-table :test #',test)
		    :iht (make-array ,size :initial-element nil)
		    :dsrcfn ,src-fn
		    :dwindfn ,windup-fn)))

(defmacro reset-row-bits (pos empty-mask mat)
  `(bit-and (aref ,mat ,pos) ,empty-mask (aref ,mat ,pos)))

(defmethod direct-update ((inst lru-cache) pos)
  (with-slots (size fmsk mat) inst
    (bit-ior (aref mat pos)
	     fmsk
	     (aref mat pos))
    (dotimes (i size)
      (declare (type fixnum i))
      (setf (aref (aref mat i) pos) 0))
    inst))

(defmethod get-lu ((inst lru-cache))
  (loop
     for i fixnum from 0
     for row simple-bit-vector across (mat inst)
     when (equal row (emsk inst))
     do (return i)))

(defmacro rplcache (inst key val &key windup)
  `(with-slots (aval ht iht default-src-fn default-windup-fn) ,inst
     (let* ((pos (get-lu ,inst))
	    (windup-res
	     (aif (aref aval pos)
		  (with-gc
		    (funcall (or ,windup default-windup-fn) it)))))
       (remhash (aref iht pos) ht)
       (setf (aref iht pos) ,key
	     (aref aval pos) (with-gc ,val)
	     (gethash ,key ht) pos)
       (direct-update ,inst pos)
       (values (aref aval pos) windup-res))))

(defmethod lru-get ((inst lru-cache) key &rest src-args
		    &key windup)
  (sb-sys:without-gcing
      (with-slots (aval ht iht default-src-fn default-windup-fn) inst
	(aif (gethash key ht)
	     (multiple-value-prog1
		 (values (aref aval it) nil)
	       (direct-update inst it))
	     (let* ((pos (get-lu inst))
		    (windup-res
		     (aif (aref aval pos)
			  (with-gc
			    (funcall (or windup default-windup-fn) it)))))
	       (remhash (aref iht pos) ht)
	       (setf (aref iht pos) key
		     (aref aval pos) (with-gc
				       (apply default-src-fn
					      (cons key src-args))) 
		     (gethash key ht) pos)
	       (direct-update inst pos)
	       (values (aref aval pos) windup-res))))))

(defmethod lru-get/src ((inst lru-cache) src-fn key &rest src-args
			&key windup)
  (sb-sys:without-gcing
      (with-slots (aval ht iht default--fn) inst
	(aif (gethash key ht)
	     (prog1
		 (values (aref aval it) nil)
	       (direct-update inst it))
	     (let* ((pos (get-lu inst))
		    (windup-res
		     (aif (aref aval pos)
			  (with-gc
			    (funcall (or windup default-windup-fn) it)))))
	       (remhash (aref iht pos) ht)
	       (setf (aref iht pos) key
		     (aref aval pos) (with-gc
				       (apply src-fn (cons key src-args))) 
		     (gethash key ht) pos)
	       (direct-update inst pos)
	       (values (aref aval pos) windup-res))))))

(defmethod lru-set ((inst lru-cache) key val &key windup)
  (sb-sys:without-gcing
      (with-slots (aval mat ht iht default-windup-fn) inst
	(aif (gethash key ht)
	     (prog1
		 (values (setf (aref aval it) val) nil)
	       (direct-update inst it))
	     (let* ((pos (get-lu inst))
		    (windup-res
		     (aif (aref aval pos)
			  (with-gc
			    (funcall (or windup default-windup-fn) it)))))
	       (remhash (aref iht pos) ht)
	       (direct-update inst pos)
	       (values (setf (aref iht pos) key
			     (gethash key ht) pos
			     (aref aval pos) val)
		       windup-res))))))

(defmethod lru-rem ((inst lru-cache) key &key windup)
  (sb-sys:without-gcing
      (with-slots (mat emsk aval ht default-windup-fn) inst
	(awhen (gethash key ht)
	       (reset-row-bits it emsk mat)
	       (setf (gethash key ht) nil)
	       (aif (aref aval it)
		    (with-gc
		      (funcall (or windup default-windup-fn) it)))))))

(defmethod lru-fclr ((inst lru-cache))
  (sb-sys:without-gcing
      (with-slots (size emsk mat aval ht iht) inst
	(loop
	   for key being the hash-keys in ht
	   do (remhash key ht))
	(loop
	   for i fixnum from 0 below size
	   do
	     (bit-and (aref mat i)
		      emsk
		      (aref mat i))))))

(defmethod lru-clr ((inst lru-cache) &key windup)
  (with-slots (size emsk mat aval ht iht default-windup-fn) inst
    (loop
       for key being the hash-keys in ht
       do (remhash key ht))
    (loop
       with windup-fn = (or windup default-windup-fn)
       for i fixnum from 0 below size
       collect (aif (aref aval i)
		    (with-gc
		      (funcall (or windup default-windup-fn) it)))
       do
	 (setf (aref aval i) nil
	       (aref iht i) nil)
	 (bit-and (aref mat i)
		  emsk
		  (aref mat i)))))

