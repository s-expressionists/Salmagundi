(cl:in-package #:asdf-user)

(defsystem "salmagundi"
  :depends-on ("quaviver"
               "trinsic")
  :serial t
  :components ((:module "code"
                :pathname "code/"
                :serial t
                :components ((:file "packages")
                             (:file "generic-functions")
                             (:file "hash-table")
                             (:file "hash")
                             (:file "interface")))))

(defsystem "salmagundi/fnv-hash"
  :depends-on ("salmagundi")
  :serial t
  :components ((:module "code"
                :pathname "code/fnv-hash/"
                :serial t
                :components ((:file "packages")
                             (:file "hash")))))

(defsystem "salmagundi/sip-hash"
  :depends-on ("salmagundi")
  :serial t
  :components ((:module "code"
                :pathname "code/sip-hash/"
                :serial t
                :components ((:file "packages")
                             (:file "hash")))))

(defsystem "salmagundi/bucket"
  :depends-on ("salmagundi")
  :serial t
  :components ((:module "code"
                :pathname "code/buckets/"
                :serial t
                :components ((:file "packages")
                             (:file "bucket-hash-table")))))

(defsystem "salmagundi/chained-hash-table"
  :depends-on ("salmagundi")
  :serial t
  :components ((:module "code"
                :pathname "code/chained-hash-table/"
                :serial t
                :components ((:file "packages")
                             (:file "hash-table")))))

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
