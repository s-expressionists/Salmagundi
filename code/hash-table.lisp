(cl:in-package #:salmagundi)

(defclass hash-table ()
  ((%test :initform #'eql
          :accessor %%hash-table-test
          :reader %hash-table-test)
   (%rehash-size :initarg :rehash-size
                 :initform 1.5
                 :reader hash-table-rehash-size)
   (%rehash-threshold :initarg :rehash-threshold
                      :initform 0.8
                      :reader hash-table-rehash-threshold)
   (%hash-function :reader hash-table-hash-function
                   :initarg :hash-function)))

(defmethod initialize-instance :after ((hash-table hash-table) &key test &allow-other-keys)
  (assert (> (hash-table-rehash-size hash-table) 1)
          ()
          "The rehash-size must be greater than 1.")
  ;; Ensure the hash table test is a function.
  (etypecase test
    (function
     (setf (%%hash-table-test hash-table) test))
    (symbol
     (setf (%%hash-table-test hash-table)
           (fdefinition (hash-table-test hash-table))))))

(defmethod hash-table-p ((object hash-table))
  t)
