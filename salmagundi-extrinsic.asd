(asdf:defsystem "salmagundi-extrinsic"
  :depends-on ("salmagundi"
               "salmagundi/bucket"
               "salmagundi/list")
  :components ((:module code
                :pathname "Code/Extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
