(cl:in-package #:salmagundi)

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
(defgeneric %hash-table-test (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

(defun hash-table-test (hash-table)
  (let ((designator (%hash-table-test hash-table)))
    (if (symbolp designator)
        designator
        (let ((test-pair (rassoc designator *standard-tests*)))
          (if (null test-pair)
              designator
              (car test-pair))))))

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

;;; Internal
(defgeneric make-hash-table-iterator (hash-table)
  (:method (hash-table)
    (error 'type-error :datum hash-table :expected-type 'hash-table)))

