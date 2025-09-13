(cl:in-package #:salmagundi-extrinsic)

(defclass extrinsic-client (salmagundi:standard-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client salmagundi/chained-hash-table:client salmagundi/fnv-hash:client)
  ())

(defvar *client* (make-instance 'extrinsic-client-impl))

(salmagundi:define-interface :client-form *client* :client-class extrinsic-client)
