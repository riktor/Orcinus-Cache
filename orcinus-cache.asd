
(in-package :cl-user)

(defpackage :orcinus-cache-asd
  (:use :cl :asdf))

(in-package :orcinus-cache-asd)

(defsystem :orcinus-cache
    :version "0.0.1"
    :author "Riku Togashi <kaminari44@gmail.com>"
    :maintainer "Riku Togashi <kaminari44@gmail.com>"
    :depends-on (#:alexandria #:anaphora)
    :serial t
    :components ((:file "packages")
		 (:file "util")
		 (:file "lru")))

(defsystem :orcinus-cache-test
    :author "Riku Togashi <kaminari44@gmail.com>"
    :maintainer "Riku Togashi <kaminari44@gmail.com>"
    :depends-on (:orcinus-cache :cl-test-more)
    :components ((:module "test"
			  :serial t
			  :components ((:file "packages")
				       (:file "tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :orcinus-cache))))
  (operate 'load-op :orcinus-cache-test)
  (funcall (intern (symbol-name :run-all-tests)
		   (find-package :orcinus-cache-test))))
