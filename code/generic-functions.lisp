(cl:in-package #:salmagundi)

(defclass standard-client () ())

(defgeneric compute-hash (client state))

(defgeneric hash (client value &optional state))

(defgeneric equivalence-hash (client equivalence value &optional state))

(defgeneric hash-table-count (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric hash-table-rehash-size (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric hash-table-rehash-threshold (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric hash-table-size (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defvar *standard-tests*
  (loop for test in '(eq eql equal equalp)
        collect (cons test (fdefinition test))))

;;; When the test is one of the four standardized hash table test
;;; functions, the value returned must be a symbol, but it would be
;;; preferable for hash table implementations to call a function
;;; instead of coercing a function to a symbol and then coercing that
;;; back to a function.
(defgeneric hash-table-test (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric gethash (key hash-table &optional default)
  (:method (key hash-table &optional default)
    (declare (ignore key default))
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric (setf gethash) (new-value key hash-table &optional default)
  (:method (new-value key hash-table &optional default)
    (declare (ignore new-value key default))
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric remhash (key hash-table)
  (:method (key hash-table)
    (declare (ignore key))
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric maphash (function hash-table)
  (:method (function hash-table)
    (declare (ignore function))
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric clrhash (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defgeneric hash-table-p (object)
  (:method (object)
    (declare (ignore object))
    nil))

(defgeneric make-hash-table (client &key))

(defgeneric default-hash-function (client name))

(defgeneric normalize-test-function (client function)
  (:method (client function)
    (declare (ignore client))
    (let ((test-pair (rassoc function *standard-tests*)))
          (if (null test-pair)
              function
              (car test-pair)))))

;;; Internal
(defgeneric make-hash-table-iterator (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

