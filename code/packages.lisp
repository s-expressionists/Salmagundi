(cl:in-package #:common-lisp-user)

(defpackage #:salmagundi
  (:use #:common-lisp)
  ;; Shadow these for now.  Ultimately, import them with
  ;; the rest of the CL package. 
  (:shadow #:clrhash
           #:gethash
           #:hash-table
           #:hash-table-count
           #:hash-table-p
           #:hash-table-rehash-size
           #:hash-table-rehash-threshold
           #:hash-table-size
           #:hash-table-test
           #:make-hash-table
           #:maphash
           #:remhash)
  (:export #:%hash-table-test
           #:clrhash
           #:define-interface
           #:eq-hash
           #:equal-hash
           #:equalp-hash
           #:find-hash-function
           #:gethash
           #:hash
           #:hash-table
           #:hash-table-count
           #:hash-table-hash-function
           #:hash-table-offset
           #:hash-table-p
           #:hash-table-rehash-size
           #:hash-table-rehash-threshold
           #:hash-table-size
           #:hash-table-test
           #:hashing-hash-table
           #:make-hash-table
           #:make-hash-table-iterator
           #:maphash
           #:remhash))
