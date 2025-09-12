(in-package #:salmagundi/sip-hash)

(deftype sip-state ()
  `(vector (unsigned-byte 64) 5))

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

(defmethod salmagundi:compute-hash ((client client) state)
  state)

(defun rotl (value count)
  (logior (ldb (byte 64 0) (ash value count))
          (ldb (byte count (- 64 count)) value)))

(defun sip-round (state count)
  (declare (type sip-state state))
  (symbol-macrolet ((v0 (aref state 0))
                    (v1 (aref state 1))
                    (v2 (aref state 2))
                    (v3 (aref state 3)))
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

(defmethod salmagundi:make-hash ((client client))
  (symbol-macrolet ((k0 (aref (key client) 0))
                    (k1 (aref (key client) 1)))
    (make-array 5
                :element-type '(unsigned-byte 64)
                :initial-contents (list (logxor k0 #x736f6d6570736575)
                                        (logxor k1 #x646f72616e646f6d)
                                        (logxor k0 #x6c7967656e657261)
                                        (logxor k1 #x7465646279746573)
                                        0))))

(defmethod salmagundi:hash ((client client) state value)
  (declare (type integer value)
           (type (or null sip-state) state))
  (symbol-macrolet ((v0 (aref state 0))
                    (v1 (aref state 1))
                    (v2 (aref state 2))
                    (v3 (aref state 3))
                    (b (aref state 4)))
    (loop for i below (integer-length value) by 64
          for word of-type (unsigned-byte 64) = (ldb (byte 64 i) value)
          do (setf v3 (logxor v3 word))
             (sip-round state (compression-count client))
             (setf v0 (logxor v0 word))
             (incf b))
    state))

(defmethod salmagundi:compute-hash ((client client) state)
  (declare (type sip-state state))
  (symbol-macrolet ((v0 (aref state 0))
                    (v1 (aref state 1))
                    (v2 (aref state 2))
                    (v3 (aref state 3))
                    (b (aref state 4)))
    (setf b (ash b 56)
          v3 (logxor v3 b))
    (sip-round state (compression-count client))
    (setf v0 (logxor v0 b)
          v2 (logxor v2 #xff))
    (sip-round state (finalization-count client))
    (ldb (byte (width client) 0) (logxor v0 v1 v2 v3))))
