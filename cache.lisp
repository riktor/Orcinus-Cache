
(in-package :orcinus-cache)

(defun get-fill-mask (size)
  (make-array size
	      :initial-element t
	      :element-type boolean))

(defun get-empty-mask (size)
  (make-array size
	      :initial-element nil
	      :element-type boolean))

(defmacro! defcache (prefix &body params-plist)
  (require-params params-plist
      (ctype     :cache-type
       csize     :cache-size
       src-fn    :src-function)
    (unless (constantp csize)
      (error "cache size must be constant fixnum."))
    (let ((fill-mask  (get-fill-mask csize))
	  (empty-mask (get-empty-mask csize)))
      (dbind (init-fn get-mac set-mac rem-mac clr-mac)
        (case ctype
	  (:lru '(lru-init lru-get lru-set lru-rem lru-clr))
	  (t (error "unknown cache type")))
	`(let ((,g!cache (,init-fn ,csize)))
	   (defun ,(symb (mkstr prefix "-get")) (key)
	     ,(macroexpand
	       `(,get-mac key ,src-fn ,fill-mask ,empty-mask ,g!cache)))
	   (defun ,(symb (mkstr prefix "-set")) (key)
	     ,(macroexpand
	       `(,set-mac key ,fill-mask ,empty-mask ,g!cache)))
	   (defun ,(symb (mkstr prefix "-rem")) (key)
	     ,(macroexpand
	      `(,rem-mac key ,empty-mask ,g!cache)))
	   (defun ,(symb (mkstr prefix "-clr")) ()
	     ,(macroexpand
	       `(,clr-mac ,empty-mask ,g!cache))))))))

(defcache test
  :cache-type   :lru
  :src-function (lambda (key)
		  (print key)))
