(asdf:defsystem "salmagundi-extrinsic"
  :depends-on ("salmagundi"
               "salmagundi/bucket"
               "salmagundi/list")
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(asdf:defsystem "salmagundi-extrinsic/test"
  :depends-on ("salmagundi-extrinsic"
               "ansi-test-harness")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :salmagundi-extrinsic/ansi-test :test))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "ansi-test")))))
