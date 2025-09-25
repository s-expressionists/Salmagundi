(asdf:defsystem "salmagundi-extrinsic"
  :depends-on ("salmagundi"
               "salmagundi/fnv-hash"
               "salmagundi/chained-hash-table")
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(asdf:defsystem "salmagundi-extrinsic/test"
  :depends-on ("salmagundi-extrinsic"
               "salmagundi/fnv-hash"
               "salmagundi/sip-hash"
               "salmagundi/jump-hash-table"
               "salmagundi/bucket"
               "salmagundi/chained-hash-table"
               "salmagundi/linear-probing"
               "ansi-test-harness")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :salmagundi-extrinsic/ansi-test :test))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "ansi-test")
                             (:static-file "expected-failures.sexp")))))

(asdf:defsystem "salmagundi-extrinsic/benchmark"
  :depends-on ("cl-spark"
               "salmagundi-extrinsic"
               "salmagundi/fnv-hash"
               "salmagundi/sip-hash"
               "salmagundi/jump-hash-table"
               "salmagundi/bucket"
               "salmagundi/chained-hash-table"
               "salmagundi/linear-probing"
               "trivial-garbage")
  :components ((:module code
                :pathname "code/extrinsic/benchmark/"
                :serial t
                :components ((:file "packages")
                             (:file "utility")
                             (:file "bench")))))
