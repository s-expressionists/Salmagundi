(asdf:defsystem "salmagundi-intrinsic"
  :depends-on ("salmagundi"
               "salmagundi/bucket"
               "trivial-package-locks")
  :components ((:module code
                :pathname "Code/Intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
