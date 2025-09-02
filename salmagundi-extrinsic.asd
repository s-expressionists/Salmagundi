(asdf:defsystem "salmagundi-extrinsic"
  :depends-on ("salmagundi"
               "salmagundi/bucket"
               "salmagundi/list")
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
