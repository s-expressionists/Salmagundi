(cl:in-package #:salmagundi-intrinsic)

(defclass intrinsic-client ()
  ())

(defclass intrinsic-client-impl (intrinsic-client salmagundi/bucket:bucket-client)
  ())

(defvar *client* (make-instance 'intrinsic-client-impl))

(salmagundi:define-interface :client-form *client* :client-class intrinsic-client :intrinsic t)
