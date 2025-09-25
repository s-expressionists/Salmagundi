(cl:in-package #:common-lisp-user)

(defpackage #:salmagundi/jump-hash-table
  (:use #:common-lisp)
  (:shadow #:hash-table)
  (:export #:client))
