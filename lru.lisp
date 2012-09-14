
(in-package :orcinus-cache)

(defstruct (lru-cache (:conc-name lruc-))
  size
  mat
  aval
  ht
  iht)

(defun lru-init (size &key (vtype nil) (test #'equal))
  (declare (type fixnum size))
  (let ((chc (make-array size
			 :element-type `(simple-bit-vector ,size))))
    (dotimes (i size)
      (setf (aref chc i)
	    (make-array size
			:element-type 'bit)))
    (make-lru-cache
     :size size
     :mat  chc
     :aval (if vtype
	       (make-array size :element-type vtype)
	       (make-array size))
     :ht  (make-hash-table :test test)
     :iht (make-array size :initial-element nil))))

(defmacro reset-row-bits (pos empty-mask mat)
  `(bit-and (aref ,mat ,pos) ,empty-mask (aref ,mat ,pos)))

(defmacro direct-update (pos mask cache)
  `(with-slots (size mat) ,cache
     (bit-ior (aref mat ,pos)
	      ,mask
	      (aref mat ,pos))
     (dotimes (i size)
       (declare (type fixnum i))
       (setf (aref (aref mat i) ,pos) 0))
     ,cache))

(defmacro get-lu (empty-mask cache)
  `(loop
      for i from 0
      for row across (lruc-mat ,cache)
      when (equal row ,empty-mask)
      do (return i)))

(defmacro lru-get (key src-fn src-args fill-mask empty-mask cache)
  `(with-slots (aval ht iht) ,cache
     (aif (gethash ,key ht)
	  (prog1
	      (aref aval it)
	    (direct-update it ,fill-mask ,cache))
	  (let ((pos (get-lu ,empty-mask ,cache)))
	    (remhash (aref iht pos) ht)
	    (setf (aref iht pos) ,key
		  (aref aval pos) (funcall ,src-fn ,key ,@src-args)
		  (gethash ,key ht) pos)
	    (direct-update pos ,fill-mask ,cache)
	    (aref aval pos)))))

(defmacro lru-set (key val fill-mask empty-mask cache)
  `(with-slots (aval mat ht iht) ,cache
     (aif (gethash ,key ht)
	  (prog1
	      (setf (aref aval it) ,val)
	    (direct-update it ,fill-mask ,cache))
	  (let ((pos (get-lu ,empty-mask ,cache)))
	    (remhash (aref iht pos) ht)
	    (direct-update pos ,fill-mask ,cache)
	    (setf (aref iht pos) ,key
		  (gethash ,key ht) pos
		  (aref aval pos) ,val)))))


(defmacro lru-rem (key mask cache)
  `(with-slots (mat aval ht) ,cache
     (awhen (gethash ,key ht)
       (reset-row-bits it ,mask mat)
       (setf (gethash ,key ht) nil)
       (aref aval it))))

(defmacro lru-clr (empty-mask cache)
 `(with-slots (size mat aval ht iht) ,cache
    (loop
       for key being the hash-keys in ht
       do (remhash key ht))
    (loop
       for i from 0 below size
       do
	 (setf (aref aval i) nil
	       (aref iht i) nil)
	 (bit-and (aref mat i)
		  ,empty-mask
		  (aref mat i)))))

(defun updtest ()
  (let ((a1 (make-array 3 :element-type '(simple-bit-vector 3))))
    #f
    (dotimes (i 3)
      (setf (aref a1 i) (make-array 3 :element-type 'bit)))
    (time
     (loop repeat 10000 do
	(bit-and (aref a1 0) #*111 (aref a1 0))
	(dotimes (i 3)
	  (setf (aref (aref a1 i) 0) 0))))

    (dotimes (i 3)
      (setf (aref a1 i) (make-array 3 :element-type 'bit)))
    (time
     (loop repeat 10000 do
	(dotimes (i 3)
	  (dotimes (j 3)
	    (setf (aref (aref a1 i) j) 1)))
	(bit-and (aref a1 0) #*111)
	(dotimes (i 3)
	  (setf (aref (aref a1 i) 0) 0))))))


