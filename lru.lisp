
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
			 :element-type (simple-bit-vector size))))
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

(defmacro get-fill-mask (cache)
  `(make-array (array-total-size (lruc-size ,cache))
	       :initial-element t
	       :element-type boolean))

(defmacro reset-row-bits (pos empty-mask mat)
  `(bit-and (aref ,mat ,pos) ,empty-mask (aref ,mat ,pos)))

(defmacro direct-update (pos mask cache)
  `(let ((mat (lruc-mat ,cache)))
     (bit-and (aref mat ,pos) ,mask (aref mat ,pos))
     (dotimes (i (lruc-size ,cache))
       (declare (type fixnum i))
       (setf (aref (aref mat i) ,pos) 0))))

(defmacro get-lu (empty-mask cache)
  `(let ((mat (lruc-mat ,cache)))
     (loop
	for i from 0
	for row across mat
	when (equal row ,empty-mask)
	  do (return i))))


(defmacro lru-get (key src-fn fill-mask empty-mask cache)
  `(aif (gethash (lruc-ht ,cache) ,key)
	(prog1
	    (aref (lruc-aval ,cache) it)
	  (direct-update it ,fill-mask ,cache))
	(let ((pos (get-lu ,empty-mask ,cache))
	      (iht  (lruc-iht ,cache))
	      (ht   (lruc-ht ,cache))
	      (aval (lruc-aval ,cache)))
	   (remhash (aref iht pos) ht)
	   (setf (aref iht pos) ,key
		 (aref aval pos) (funcall ,src-fn ,key)
		 (gethash ,key ht) pos)
	   (aref aval pos))))

(defmacro lru-set (key val fill-mask empty-mask cache)
  `(aif (gethash (lruc-ht ,cache) ,key)
	(prog1
	    (setf (lruc-aval ,cache) it)
	  (direct-update it ,fill-mask ,cache))
	(progn
	  (lru-rem ,key ,empty-mask (lruc-mat ,cache))
	  
	  )
	
	)

  )


(defmacro lru-rem (key mask cache)
  `(awhen (gethash ,key (lruc-ht ,cache))
	  (let ((mat (lruc-mat cache)))
	    (reset-row-bits it ,mask mat)
	    (setf (gethash ,key (lruc-ht ,cache)) nil))))

(defmacro lru-clr (empty-mask cache)
 `(progn
    (loop
       for key being the hash-keys in (lruc-ht ,cache)
       do (remhash key (lruc-ht ,cache)))
    (loop
       with mat = (lruc-mat ,cache)
       for i from 0 below (lruc-size ,cache)
       do
	 (setf (aref (lruc-aval ,cache) i) nil
	       (aref (lruc-iht ,cache) i) nil)
	 (bit-and (aref mat i) empty-mask (aref mat i)))))


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


