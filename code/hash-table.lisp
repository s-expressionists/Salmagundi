(cl:in-package #:salmagundi)

(defclass hash-table ()
  ((%test :reader hash-table-test
          :initarg :test
          :initform 'eql)
   (%rehash-size :initarg :rehash-size
                 :initform 1.5
                 :reader hash-table-rehash-size)
   (%rehash-threshold :initarg :rehash-threshold
                      :initform 0.8
                      :reader hash-table-rehash-threshold)
   (%hash-function :reader hash-table-hash-function
                   :initarg :hash-function)))

(defmethod hash-table-p ((object hash-table))
  t)

(defmethod make-hash-table :around
    ((client standard-client) &rest initargs
     &key (test 'eql) (size nil size-p) (rehash-size nil rehash-size-p)
          (rehash-threshold nil rehash-threshold-p) (hash-function nil hash-function-p))
  (check-type test (or symbol function))
  (setf test (normalize-test-function client test)
        (getf initargs :test) test)
  (when size-p
    (check-type size (integer 0 *))
    (setf (getf initargs :size) size))
  (when rehash-size-p
    (check-type rehash-size (or (integer 1 *) (float (1.0) *)))
    (setf (getf initargs :rehash-size) rehash-size))
  (when rehash-threshold-p
    (check-type rehash-threshold (real 0 1))
    (setf (getf initargs :rehash-threshold) rehash-threshold))
  (unless hash-function-p
    (setf hash-function (default-hash-function client test)))
  (check-type hash-function (or symbol function))
  (setf (getf initargs :hash-function) hash-function)
  (apply #'call-next-method client initargs))
