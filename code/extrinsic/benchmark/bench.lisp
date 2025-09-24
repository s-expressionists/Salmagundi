(in-package #:salmagundi-extrinsic/benchmark)

(define-benchmark hash-integers
  (loop with table = (salmagundi-extrinsic:make-hash-table );:hash-function 'sb-impl::eql-hash)
        for i below 100000
        finally (salmagundi-extrinsic:maphash
                 (lambda (key value)
                   (incf (salmagundi-extrinsic:gethash key table) value))
                 table)
        do (setf (salmagundi-extrinsic:gethash i table) i)))
