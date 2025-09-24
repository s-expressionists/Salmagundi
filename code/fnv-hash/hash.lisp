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
  (let ((hash (initial-hash client)))
    (declare (type fixnum hash))
    (values (lambda (value &aux (i 0) (l (integer-length value)))
              (declare (type integer value)
                       (type fixnum i l))
              (tagbody
               next
                 (when (< i l)
                   (setf hash (ldb (byte +width+ 0)
                                   (* (logxor hash (ldb (byte 8 i) value))
                                      +prime+)))
                   (incf i 8)
                   (go next))))
            (lambda ()
              hash))))

(defmethod salmagundi:hash ((client client) function object)
  (let ((hash (initial-hash client)))
    (declare (type fixnum hash))
    (funcall function
             (lambda (value &aux (i 0) (l (integer-length value)))
               (declare (type fixnum i l))
               (tagbody
                next
                  (when (< i l)
                    (setf hash (ldb (byte +width+ 0)
                                    (* (logxor hash (ldb (byte 8 i) value))
                                       +prime+)))
                    (incf i 8)
                    (go next))))
            object)
    hash))

#|(defmacro with-hash (func object-var)
  `(let ((hash 0))
     (declare (type fixnum hash))
     (,func (lambda (value &aux (i 0) (l (integer-length value)))
              (declare (type fixnum i l))
              (tagbody
               next
                 (when (< i l)
                   (setf hash (ldb (byte +width+ 0)
                                   (* (logxor hash (ldb (byte 8 i) value))
                                      +prime+)))
                   (incf i 8)
                   (go next))))
            ,object-var)
     hash))

(defun eq-hash (object)
  (with-hash salmagundi:eq-hash object))

(defun eql-hash (object)
  (with-hash salmagundi:eql-hash object))

(defmethod salmagundi:default-hash-function ((client client) (name (eql 'eq)))
  'eq-hash)

(defmethod salmagundi:default-hash-function ((client client) (name (eql 'eql)))
  'eql-hash)
|#
