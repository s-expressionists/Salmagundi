(in-package #:salmagundi/sip-hash)

(defclass client ()
  ((key :accessor key
        :initarg :key
        :initform (make-array 2 :element-type '(unsigned-byte 64)
                                :initial-contents (list (random (ash 1 64))
                                                        (random (ash 1 64))))
        :type (vector (unsigned-byte 64) 2))
   (compression-count :accessor compression-count
                      :initarg :compression-count
                      :initform 2
                      :type fixnum)
   (finalization-count :accessor finalization-count
                      :initarg :finalization-count
                      :initform 4
                      :type fixnum)
   (width :accessor width
          :initarg :width
          :initform (integer-length most-positive-fixnum)
          :type fixnum)))

(defun rotl (value count)
  (logior (ldb (byte 64 0) (ash value count))
          (ldb (byte count (- 64 count)) value)))

(defmethod salmagundi:make-hash ((client client))
  (let* ((k0 (aref (key client) 0))
         (k1 (aref (key client) 1))
         (v0 (logxor k0 #x736f6d6570736575))
         (v1 (logxor k1 #x646f72616e646f6d))
         (v2 (logxor k0 #x6c7967656e657261))
         (v3 (logxor k1 #x7465646279746573))
         (b 0))
    (flet ((sip-round (count)
             (loop repeat count
                   do (setf v0 (ldb (byte 64 0) (+ v0 v1))
                            v1 (rotl v1 13)
                            v1 (logxor v1 v0)
                            v0 (rotl v0 32)
                            v2 (ldb (byte 64 0) (+ v2 v3))
                            v3 (rotl v3 16)
                            v3 (logxor v3 v2)
                            v0 (ldb (byte 64 0) (+ v0 v3))
                            v3 (rotl v3 21)
                            v3 (logxor v3 v0)
                            v2 (ldb (byte 64 0) (+ v2 v1))
                            v1 (rotl v1 17)
                            v1 (logxor v1 v2)
                            v2 (rotl v2 32)))))
      (values (lambda (value)
                (declare (type integer value))
                (loop for i below (integer-length value) by 64
                      for word of-type (unsigned-byte 64) = (ldb (byte 64 i) value)
                      do (setf v3 (logxor v3 word))
                         (sip-round (compression-count client))
                         (setf v0 (logxor v0 word))
                         (incf b)))
              (lambda ()
                (setf b (ash b 56)
                      v3 (logxor v3 b))
                (sip-round (compression-count client))
                (setf v0 (logxor v0 b)
                      v2 (logxor v2 #xff))
                (sip-round (finalization-count client))
                (ldb (byte (width client) 0) (logxor v0 v1 v2 v3)))))))
