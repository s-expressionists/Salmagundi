(cl:defpackage #:salmagundi-extrinsic
  (:use #:common-lisp)
  (:nicknames #:se)
  (:shadow #:make-hash-table
           #:with-hash-table-iterator)
  (:export #:*client*
           #:extrinsic-client))
