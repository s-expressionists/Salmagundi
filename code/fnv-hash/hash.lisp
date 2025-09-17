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
  (declare (type (vector fixnum 1) state))
  (aref state 0))

(defmethod salmagundi:hash ((client client) state (value integer))
  (declare (type (vector fixnum 1) state))
  (prog ((i 0)
         (l (integer-length value))
         (s (aref state 0)))
     (declare (type fixnum i l s))
   next
     (when (< i l)
       (setf s (ldb (byte +width+ 0) (* (logxor s (ldb (byte 8 i) value)) +prime+)))
       (incf i 8)
       (go next))
     (setf (aref state 0) s)))
