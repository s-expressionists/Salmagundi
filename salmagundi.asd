(cl:in-package #:asdf-user)

(defsystem "salmagundi"
  :serial t
  :components ((:module "code"
                :pathname "code/"
                :serial t
                :components ((:file "packages")
                             (:file "generic-functions")
                             (:file "hash-table")
                             (:file "sxhash")
                             (:file "hashing-hash-table")
                             (:file "interface")))))

(defsystem "salmagundi/bucket"
  :depends-on ("salmagundi")
  :serial t
  :components ((:module "code"
                :pathname "code/buckets/"
                :serial t
                :components ((:file "packages")
                             (:file "bucket-hash-table")))))

(defsystem "salmagundi/linear-probing"
  :depends-on ("salmagundi")
  :serial t
  :components ((:module "code"
                :pathname "code/linear-probing/"
                :serial t
                :components ((:file "packages")
                             (:file "metadata-table")
                             (:file "linear-probing-hash-table")))))

(defsystem "salmagundi/list"
  :depends-on ("salmagundi")
  :serial t
  :components ((:module "code"
                :pathname "code/list/"
                :serial t
                :components ((:file "packages")
                             (:file "list-hash-table")))))
