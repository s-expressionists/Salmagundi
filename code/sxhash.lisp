(cl:in-package #:salmagundi)

(defconstant +fnv-prime+ 1099511628211)

(defparameter *hash-offset* (random (expt 2 62)))

(declaim (inline %fnv-1a fnv-1a))

(defun %fnv-1a (last-hash byte)
  (declare ((unsigned-byte 8) byte)
           ((unsigned-byte 62) last-hash))
  (ldb (byte 62 0)
       (logxor (* last-hash +fnv-prime+)
               byte)))

(defun fnv-1a (last-hash &rest bytes)
  (declare ((unsigned-byte 62) last-hash))
  (reduce #'%fnv-1a bytes :initial-value last-hash))

(define-compiler-macro fnv-1a (last-hash &rest bytes)
  (reduce (lambda (last-form argument)
            `(%fnv-1a ,last-form ,argument))
          bytes
          :initial-value last-hash))

(defun eq-hash (object &optional (last-hash *hash-offset*))
  (typecase object
    ;; Instances of a built-in-class can't change-class, so we can
    ;; gather some entropy from their classes at the very least.
    (cons
     (fnv-1a last-hash 1))
    (character
     (let ((code (char-code object)))
       (fnv-1a last-hash
               2
               (ldb (byte 8 0) code)
               (ldb (byte 8 8) code))))
    (integer
     (fnv-1a last-hash
             3
             (ldb (byte 8 0) object)
             (ldb (byte 8 8) object)
             (ldb (byte 8 16) object)
             (ldb (byte 8 24) object)))
    (float
     (multiple-value-bind (significand exponent)
         (integer-decode-float object)
       (fnv-1a last-hash
               4
               (ldb (byte 8 0) significand)
               (ldb (byte 8 8) significand)
               (ldb (byte 8 16) significand)
               (ldb (byte 8 0) exponent))))
    ;; The element-type of an array won't change.
    (array
     (fnv-1a (equal-hash (array-element-type object) last-hash)
             2))
    (symbol
     (equal-hash (symbol-name object) last-hash))
    (t
     last-hash)))

(defvar *depth* 3)

(defun equal-hash (object &optional (last-hash *hash-offset*))
  (when (zerop *depth*)
    (return-from equal-hash last-hash))
  (typecase object
    (cons
     (let ((*depth* (1- *depth*)))
       (equal-hash (cdr object) (equal-hash (car object) last-hash))))
    (symbol
     (equal-hash (symbol-name object) (fnv-1a last-hash 1)))
    (string
     (loop for char across object
           for position below 16
           for hash = last-hash then (equal-hash char hash)
           finally (return hash)))
    (t
     (eq-hash object last-hash))))

(defun equalp-hash (object &optional (last-hash *hash-offset*))
  (when (zerop *depth*)
    (return-from equalp-hash last-hash))
  (typecase object
    (cons
     (let ((*depth* (1- *depth*)))
       (equalp-hash (cdr object) (equalp-hash (car object) last-hash))))
    (symbol
     (equal-hash (symbol-name object) (fnv-1a last-hash 1)))
    (character
     (eq-hash (char-downcase object) last-hash))
    (string
     (loop for char across object
           for position below 16
           for hash = last-hash then (equalp-hash char hash)
           finally (return hash)))
    (array
     (loop with size = (array-total-size object)
           for position below size
           for hash = last-hash
             then (equalp-hash (row-major-aref object position) hash)
           finally (return hash)))
    (t (eq-hash object last-hash))))

(defvar *sxhash-offset* 14695981039346656037)

#+(or)
(defun sxhash (object)
  (ldb (byte 62 0)
       (equal-hash *sxhash-offset* object)))

(defmethod default-hash-function ((client standard-client) (name (eql 'eq)))
  'eq-hash)

(defmethod default-hash-function ((client standard-client) (name (eql 'eql)))
  'eq-hash)

(defmethod default-hash-function ((client standard-client) (name (eql 'equal)))
  'equal-hash)

(defmethod default-hash-function ((client standard-client) (name (eql 'equalp)))
  'equalp-hash)

(defmethod make-hash-table :around ((client standard-client) &rest initargs &key (test 'eql) (hash-function nil hash-function-p))
  (declare (ignore hash-function))
  (if hash-function-p
      (call-next-method)
      (apply #'call-next-method client :hash-function (default-hash-function client test) initargs)))
