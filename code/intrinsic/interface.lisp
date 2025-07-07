(cl:in-package #:salmagundi-intrinsic)

(defclass intrinsic-client ()
  ())

(defclass intrinsic-client-impl (intrinsic-client salmagundi/bucket:bucket-client)
  ())

(defvar *client* (make-instance 'intrinsic-client-impl))

(trivial-package-locks:with-unlocked-system-packages
  (salmagundi:define-interface *client* intrinsic-client t))
