(in-package #:salmagundi/fnv-hash)

(defconstant +prime+ #x100000001b3)

(defconstant +width+ (integer-length most-positive-fixnum))

(defclass client ()
  ((variation :reader variation
              :initarg :variation
              :initform :1a
              :type (member :1 :1a))
   (initial-hash :accessor initial-hash
                 :initarg :initial-hash
                 :initform (random most-positive-fixnum)
                 :type fixnum)))

(defmethod salmagundi:make-hash ((client client))
  (make-array 1 :element-type 'fixnum :initial-element (initial-hash client)))

(defmethod salmagundi:compute-hash ((client client) state)
  (aref state 0))

(defmethod salmagundi:hash ((client client) state (value integer))
  (declare (type (vector fixnum 1) state))
  (loop for i below (integer-length value) by 8
        do (setf (aref state 0)
                 (ldb (byte +width+ 0) (* (logxor (aref state 0)
                                                  (ldb (byte 8 i) value))
                                          +prime+)))))
