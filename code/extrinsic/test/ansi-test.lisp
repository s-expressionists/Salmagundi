(in-package #:salmagundi-extrinsic/ansi-test)

(defclass bucket/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/bucket:bucket-client
     salmagundi/fnv-hash:client)
  ())

(defclass bucket/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/bucket:bucket-client
     salmagundi/sip-hash:client)
  ())

(defclass chained/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/chained-hash-table:client
     salmagundi/fnv-hash:client)
  ())

(defclass chained/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/chained-hash-table:client
     salmagundi/sip-hash:client)
  ())

(defclass linear-probing/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/linear-probing:linear-probing-client
     salmagundi/fnv-hash:client)
  ())

(defclass linear-probing/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/linear-probing:linear-probing-client
     salmagundi/sip-hash:client)
  ())

(defun test (&rest args)
  (let ((system (asdf:find-system :salmagundi-extrinsic/test)))
    (apply #'ansi-test-harness:ansi-test
           :directory (merge-pathnames (make-pathname
                                        :directory '(:relative "dependencies" "ansi-test"))
                                       (asdf:component-pathname system))
           :extrinsic-symbols '(salmagundi-extrinsic:hash-table
                                salmagundi-extrinsic:make-hash-table
                                salmagundi-extrinsic:hash-table-p
                                salmagundi-extrinsic:hash-table-count
                                salmagundi-extrinsic:hash-table-rehash-size
                                salmagundi-extrinsic:hash-table-rehash-threshold
                                salmagundi-extrinsic:hash-table-size
                                salmagundi-extrinsic:hash-table-test
                                salmagundi-extrinsic:gethash
                                salmagundi-extrinsic:remhash
                                salmagundi-extrinsic:maphash
                                salmagundi-extrinsic:with-hash-table-iterator
                                salmagundi-extrinsic:sxhash
                                salmagundi-extrinsic:clrhash)
           :expected-failures (asdf:component-pathname
                               (asdf:find-component
                                system
                                '("code" "expected-failures.sexp")))
           :tests '("CLRHASH"
                    "GETHASH"
                    "HASH-TABLE"
                    "MAKE-HASH-TABLE"
                    "MAPHASH"
                    "REMHASH"
                    "SXHASH"
                    "WITH-HASH-TABLE-ITERATOR")
           args)))

(defun test-all (&key exit)
  (loop with successp = t
        for client in '(bucket/fnv bucket/sip chained/fnv chained/sip linear-probing/fnv linear-probing/sip)
        for salmagundi-extrinsic:*client* = (make-instance client)
        finally (if exit
                    (uiop:quit (if successp 0 1))
                    successp)
        do (format t "Testing with ~a.~%" client)
           (setf successp (or (test) successp))))
