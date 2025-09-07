(cl:defpackage #:salmagundi-intrinsic
  (:use #:common-lisp)
  (:export #:*client*
           #:eq-hash
           #:eql-hash
           #:equal-hash
           #:equalp-hash
           #:define-hash-table-test
           #:intrinsic-client))
