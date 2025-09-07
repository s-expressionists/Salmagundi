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

(defmethod initialize-instance :after ((hash-table hash-table) &key test)
  (assert (>= (hash-table-rehash-size hash-table) 1)
          ()
          "The rehash-size must be greater than or equal to 1.")
  ;; Ensure the hash table test is a function.
  #+(or)(etypecase test
    (function
     (setf (%%hash-table-test hash-table) test))
    (symbol
     (setf (%%hash-table-test hash-table)
           (fdefinition (hash-table-test hash-table))))))

(defmethod hash-table-p ((object hash-table))
  t)

(defmethod make-hash-table :around ((client standard-client) &rest initargs &key (test 'eql) (hash-function nil hash-function-p))
  (declare (ignore hash-function))
  (setf test (normalize-test-function client test)
        (getf initargs :test) test)
  (if hash-function-p
      (apply #'call-next-method client initargs)
      (apply #'call-next-method client :hash-function (default-hash-function client test) initargs)))
