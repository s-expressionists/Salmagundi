(cl:defpackage #:salmagundi-extrinsic
  (:use #:common-lisp)
  (:nicknames #:se)
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
           #:with-hash-table-iterator)
  (:export #:*client*
           #:clrhash
           #:extrinsic-client
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
           #:with-hash-table-iterator))
