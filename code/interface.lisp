(cl:in-package #:salmagundi)

(defun check-hash-table (object)
  (unless (hash-table-p object)
    (error 'type-error :datum object :expected-type 'hash-table)))

(trinsic:make-define-interface (:intrinsic intrinsicp :client-class client-class :client-form client-var)
    ((define-hash-table-test-sym #:define-hash-table-test)
     (make-hash-table-sym cl:make-hash-table)
     (sxhash-sym cl:sxhash)
     (eq-hash-sym #:eq-hash)
     (eql-hash-sym #:eql-hash)
     (equal-hash-sym #:equal-hash)
     (equalp-hash-sym #:equalp-hash)
     (eq-hash-var #:*eq-hash* :variable t)
     (eql-hash-var #:*eql-hash* :variable t)
     (equal-hash-var #:*equal-hash* :variable t)
     (equalp-hash-var #:*equalp-hash* :variable t)
     (similarp-hash-var #:*similarp-hash* :variable t)
     (with-hash-table-iterator-sym cl:with-hash-table-iterator))
   (let* ((intrinsic-pkg (if intrinsicp "COMMON-LISP" (package-name *package*))))
     `((shadowing-import '(clrhash
                           gethash
                           hash-table
                           hash-table-count
                           hash-table-p
                           hash-table-rehash-size
                           hash-table-rehash-threshold
                           hash-table-size
                           hash-table-test
                           maphash
                           remhash)
                         ,intrinsic-pkg)

       (export '(clrhash
                 gethash
                 hash-table
                 hash-table-count
                 hash-table-p
                 hash-table-rehash-size
                 hash-table-rehash-threshold
                 hash-table-size
                 hash-table-test
                 maphash
                 remhash)
               ,intrinsic-pkg)

       (defvar ,eq-hash-var (make-instance 'eq-hash))

       (defvar ,eql-hash-var (make-instance 'eql-hash))

       (defvar ,equal-hash-var (make-instance 'equal-hash))

       (defvar ,equalp-hash-var (make-instance 'equalp-hash))

       (defvar ,similarp-hash-var (make-instance 'similarp-hash))

       (defun ,make-hash-table-sym
           (&rest rest
            &key test size rehash-size rehash-threshold hash-function
            &allow-other-keys)
         (declare (ignore test size rehash-size rehash-threshold hash-function))
         "Creates and returns a new hash table.

TEST determines how keys are compared. An object is said to be present in the hash-table if that
object is the same under the test as the key for some entry in the hash-table.

SIZE is a hint to the implementation about how much initial space to allocate in the
hash-table. This information, taken together with the REHASH-THRESHOLD, controls the approximate
number of entries which it should be possible to insert before the table has to grow. The actual
size might be rounded up from size to the next ‘good’ size; for example, some implementations
might round to the next prime number.

REHASH-SIZE specifies a minimum amount to increase the size of the hash-table when it becomes
full enough to require rehashing; see REHASH-THESHOLD below. If REHASH-SIZE is an integer, the
expected growth rate for the table is additive and the integer is the number of entries to add;
if it is a float, the expected growth rate for the table is multiplicative and the float is the
ratio of the new size to the old size. As with size, the actual size of the increase might be
rounded up.

REHASH-THRESHOLD specifies how full the hash-table can get before it must grow. It specifies the
maximum desired hash-table occupancy level.

HASH-FUNCTION specifies how hash values are computed. If unsupplied, a hash function based on
the test argument is used, which then must be one of the standardized hash table test functions,
or one for which a default hash function has been defined using
sb-ext:define-hash-table-test. If hash-function is specified, the test argument can be any two
argument predicate consistent with it. The hash-function is expected to return a non-negative
fixnum hash code. If test is neither standard nor defined by DEFINE-HASH-TABLE-TEST, then the
hash-function must be specified."
         (apply #'make-hash-table ,client-var rest))

       (defun ,sxhash-sym (object)
         (compute-hash ,client-var (equivalence-hash ,client-var ,similarp-hash-var object)))

       (defmacro ,with-hash-table-iterator-sym
           ((name hash-table) &body body)
         (let ((iterator-var (gensym "ITERATOR")))
           `(let ((,iterator-var (make-hash-table-iterator ,hash-table)))
              (macrolet ((,name () `(funcall ,',iterator-var)))
                ,@body))))

       (defun ,eq-hash-sym (object)
         (compute-hash ,client-var (equivalence-hash ,client-var ,eq-hash-var object)))

       (defmethod default-hash-function ((client ,client-class) (name (eql 'eq)))
         ',eq-hash-sym)

       (defun ,eql-hash-sym (object)
         (compute-hash ,client-var (equivalence-hash ,client-var ,eql-hash-var object)))

       (defmethod default-hash-function ((client ,client-class) (name (eql 'eql)))
         ',eql-hash-sym)

       (defun ,equal-hash-sym (object)
         (compute-hash ,client-var (equivalence-hash ,client-var ,equal-hash-var object)))

       (defmethod default-hash-function ((client ,client-class) (name (eql 'equal)))
         ',equal-hash-sym)

       (defun ,equalp-hash-sym (object)
         (compute-hash ,client-var (equivalence-hash ,client-var ,equalp-hash-var object)))

       (defmethod default-hash-function ((client ,client-class) (name (eql 'equalp)))
         ',equalp-hash-sym)

       (defmacro ,define-hash-table-test-sym (name hash-function)
         (list 'defmethod 'default-hash-function
               (list '(client ,client-class) `(name (eql ',name)))
               (list 'quote hash-function))))))
