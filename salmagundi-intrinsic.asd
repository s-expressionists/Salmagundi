(asdf:defsystem "salmagundi-intrinsic"
  :depends-on ("salmagundi"
               "salmagundi/bucket"
               "trivial-package-locks")
  :components ((:module code
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
