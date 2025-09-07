(in-package #:salmagundi-extrinsic/ansi-test)

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
