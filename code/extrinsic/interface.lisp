(cl:in-package #:salmagundi-extrinsic)

(defclass extrinsic-client ()
  ())

(defclass extrinsic-client-impl (extrinsic-client salmagundi/bucket:bucket-client)
  ())

(defvar *client* (make-instance 'extrinsic-client-impl))

(salmagundi:define-interface :client-form *client* :client-class extrinsic-client)
