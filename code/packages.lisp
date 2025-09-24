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
           #:remhash
           #:sxhash)
  (:export #:ceiling-pow2
           #:clrhash
           #:compute-hash
           #:default-hash-function
           #:define-interface
           #:define-hash-table-test
           #:eq-hash
           #:eql-hash
           #:equal-hash
           #:equalp-hash
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
           #:make-hash
           #:make-hash-table
           #:make-hash-table-iterator
           #:maphash
           #:mod-pow2
           #:rehash
           #:rehash-p
           #:remhash
           #:standard-client))
