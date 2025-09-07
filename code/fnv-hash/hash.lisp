(in-package #:salmagundi/fnv-hash)

(defclass client ()
  ((prime :accessor prime
          :initarg :prime
          :type fixnum)
   (width :accessor width
          :initarg :width
          :initform (integer-length most-positive-fixnum)
          :type fixnum)
   (byte-spec :accessor byte-spec)
   (initial-hash :accessor initial-hash
                 :initarg :initial-hash
                 :type fixnum)))


(defmethod initialize-instance :after ((instance client) &rest initargs &key)
  (declare (ignore initargs))
  (with-accessors ((prime prime)
                   (width width)
                   (byte-spec byte-spec)
                   (initial-hash initial-hash))
      instance
    (setf byte-spec (byte width 0))
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

(defmethod salmagundi:hash ((client client) value &optional hash)
  (unless hash
    (setf hash (initial-hash client)))
  (let ((prime (prime client))
        (byte-spec (byte-spec client)))
    (flet ((accumulate (byte)
             (setf hash (ldb byte-spec (* (logxor hash byte) prime)))))
      (accumulate (if (minusp value) 1 0))
      (setf value (abs value))
      (tagbody
       next
         (unless (zerop value)
           (accumulate (ldb (byte 8 0) value))
           (setf value (ash value -8))
           (go next)))))
  hash)
