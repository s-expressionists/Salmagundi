(in-package #:salmagundi/fnv-hash)

(defclass client ()
  ((prime :accessor prime
          :initarg :prime
          :type fixnum)
   (width :accessor width
          :initarg :width
          :initform (integer-length most-positive-fixnum)
          :type fixnum)
   (variation :reader variation
              :initarg :variation
              :initform :1a
              :type (member :1 :1a))
   (initial-hash :accessor initial-hash
                 :initarg :initial-hash
                 :type fixnum)))

(defmethod initialize-instance :after ((instance client) &rest initargs &key)
  (declare (ignore initargs))
  (with-accessors ((prime prime)
                   (width width)
                   (initial-hash initial-hash))
      instance
    (unless (slot-boundp instance 'initial-hash)
      (setf initial-hash (random (expt 2 width))))
    (unless (slot-boundp instance 'prime)
      (setf prime
            (cond ((<= width 32)
                   #x1000193)
                  ((<= width 64)
                   #x100000001b3)
                  ((<= width 128)
                   #x1000000000000000000013b)
                  ((<= width 256)
                   #x1000000000000000000000000000000000000000163)
                  ((<= width 512)
                   #x100000000000000000000000000000000000000000000000000000000000000000000000000000000000157)
                  ((<= width 1024)
                   #x0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000018d)
                  (t
                   (error "Unable to determine prime for FNV hash with a width of ~a."
                          width)))))))

(defmethod salmagundi:make-hash ((client client))
  (make-array 1 :element-type '(unsigned-byte 64) :initial-element (initial-hash client)))

(defmethod salmagundi:compute-hash ((client client) state)
  (ldb (byte (width client) 0) (aref state 0)))

(defmethod salmagundi:hash ((client client) state value)
  (loop with prime = (prime client)
        with variation = (variation client)
        for i below (integer-length value) by 8
        for byte of-type (unsigned-byte 8) = (ldb (byte 8 i) value)
        do (setf (aref state 0)
                 (ldb (byte 64 0)
                      (if (eq variation :1)
                          (logxor (* (aref state 0) prime) byte)
                          (* (logxor (aref state 0) byte) prime))))))
